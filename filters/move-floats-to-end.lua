--[[
  move-floats-to-end.lua

  Journal-manuscript layout: move every figure and table out of the body into a
  "Figures and Tables" section at the very end of the document (after the
  reference list), leaving an italic placeholder in the body that repeats the
  caption so a typesetter can see where each float belongs.

  Implementation note. Quarto represents a captioned, cross-referenceable float
  as a custom AST node whose on-AST form is a *scaffold Div* carrying
  __quarto_custom_type = "FloatRefTarget"; the real content lives in a registry
  resolved at write time. Two obvious approaches both fail:

    * a `Div` handler never fires for them -- Quarto's runtime intercepts the
      traversal and dispatches to a `FloatRefTarget` handler instead;
    * a `FloatRefTarget` handler does fire, but re-inserting the node object it
      hands you loses the registry link and the float vanishes from the output.

  So we do the work in `Pandoc`, walking the raw block list, where the floats
  are still plain scaffold Divs and can be relocated untouched.

  Relative order within each float type is preserved, so Quarto assigns exactly
  the same numbers it would have in the body and every @fig-/@tbl- reference
  still resolves correctly.
]]

local function is_float(blk)
  return blk.t == "Div" and blk.attributes
     and blk.attributes["__quarto_custom_type"] == "FloatRefTarget"
end

-- Does this block contain an element of the given type anywhere inside it?
local function contains(blk, want)
  local found = false
  local filter = {}
  filter[want] = function() found = true end
  pandoc.walk_block(blk, filter)
  return found
end

-- Is this float a table? A pandoc Table for docx/html, but kableExtra emits raw
-- LaTeX for PDF, so also look for tabular markup inside raw blocks.
local function is_table(blk)
  if contains(blk, "Table") then return true end
  local found = false
  local function scan(r)
    local t = r.text or ""
    if t:match("tabular") or t:match("\begin{table") or t:match("<table") then
      found = true
    end
  end
  pandoc.walk_block(blk, { RawBlock = scan, RawInline = scan })
  return found
end

-- The caption is the scaffold child holding neither an image nor a table.
local function caption_inlines(div)
  for _, child in ipairs(div.content) do
    if not contains(child, "Image") and not is_table(child) then
      local out = {}
      pandoc.walk_block(child, {
        Plain = function(p) for _, i in ipairs(p.content) do out[#out+1] = i end end,
        Para  = function(p) for _, i in ipairs(p.content) do out[#out+1] = i end end,
      })
      if #out > 0 then return out end
    end
  end
  return {}
end

local function placeholder(label, cap)
  local content = { pandoc.Str("[" .. label) }
  if #cap > 0 then
    content[#content+1] = pandoc.Str(":")
    content[#content+1] = pandoc.Space()
    for _, inl in ipairs(cap) do content[#content+1] = inl end
  end
  for _, inl in ipairs({
    pandoc.Space(), pandoc.Str("\u{2014}"), pandoc.Space(),
    pandoc.Str("about"), pandoc.Space(), pandoc.Str("here]")
  }) do content[#content+1] = inl end
  return pandoc.Para({ pandoc.Emph(content) })
end

function Pandoc(doc)
  local floats = {}
  local counts = { Figure = 0, Table = 0 }

  -- Recurse so floats nested inside other divs are caught too.
  local function process(blocks)
    local out = {}
    for _, blk in ipairs(blocks) do
      if is_float(blk) then
        local ftype = is_table(blk) and "Table" or "Figure"
        counts[ftype] = counts[ftype] + 1
        local label = ftype .. " " .. counts[ftype]
        floats[#floats+1] = blk
        out[#out+1] = placeholder(label, caption_inlines(blk))
        io.stderr:write(("[move-floats-to-end] collected %s\n"):format(label))
      else
        if blk.t == "Div" and blk.content then blk.content = process(blk.content) end
        out[#out+1] = blk
      end
    end
    return out
  end

  doc.blocks = process(doc.blocks)

  if #floats == 0 then
    io.stderr:write("[move-floats-to-end] WARNING: no floats found\n")
    return doc
  end

  doc.blocks[#doc.blocks+1] =
    pandoc.Header(1, pandoc.Inlines("Figures and Tables"),
                  pandoc.Attr("figures-and-tables", { "unnumbered" }))
  for _, f in ipairs(floats) do doc.blocks[#doc.blocks+1] = f end

  io.stderr:write(("[move-floats-to-end] moved %d floats to the end\n"):format(#floats))
  return doc
end
