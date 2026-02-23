lua<<EOF
print("test")
local nvim_lsp = require("lspconfig")
yt_lsp_update_settings("hls", {
  filetypes={"haskell", "lhaskell" },
  cmd={ "haskell-language-server-wrapper", "--logfile", "/tmp/hls2.log", "--lsp" },
  root_dir = function(bufnr, on_dir)
    local fname = vim.api.nvim_buf_get_name(bufnr)
    on_dir(nvim_lsp.util.root_pattern('hie.yaml', 'stack.yaml', 'cabal.project', '*.cabal', 'package.yaml')(fname))
  end,
  settings = {
    haskell = {
      -- formattingProvider = "ormolu"
      formattingProvider = "fourmolu"
    }
  }
})
EOF
