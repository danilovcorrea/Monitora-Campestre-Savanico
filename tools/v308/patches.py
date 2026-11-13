"""Integra apenas apresentacao/checagem de paginas; estatisticas ficam intactas."""


def apply(c, helpers):
    def gerar(text):
        text = helpers.unique(text,
            "  graficos <- data.table::copy(graficos)\n",
            "  graficos <- data.table::copy(graficos)\n"
            "  graficos <- monitora_v308_completar_indice_graficos(graficos, dir_relatorio)\n")
        text = helpers.unique(text,
            "    chaves_conteudo <- as.character(graficos$hash_md5[idx_inferenciais_disponiveis])",
            "    chaves_conteudo <- as.character(graficos$hash_md5_conjunto_paginas[idx_inferenciais_disponiveis])")
        text = helpers.unique(text,
            '  texto_detalhado_gate <- paste(conteudo_detalhado, collapse = "\\n")',
            "  conteudo_detalhado <- monitora_v308_expandir_figuras(conteudo_detalhado, dir_relatorio)\n"
            "  conteudo_sintetico <- monitora_v308_expandir_figuras(conteudo_sintetico, dir_relatorio)\n"
            "  monitora_v308_validar_paginas_documentais(graficos, dir_relatorio,\n"
            "    conteudo_detalhado, conteudo_sintetico)\n"
            '  texto_detalhado_gate <- paste(conteudo_detalhado, collapse = "\\n")')
        text = helpers.unique(text,
            "      .(painel_representante = id, hash_representante = hash_md5)",
            "      .(painel_representante = id, hash_representante = hash_md5,\n"
            "        hash_conjunto_representante = hash_md5_conjunto_paginas)")
        text = helpers.unique(text,
            "        duplicados$hash_md5 != duplicados$hash_representante |",
            "        duplicados$hash_md5 != duplicados$hash_representante |\n"
            "        is.na(duplicados$hash_conjunto_representante) |\n"
            "        duplicados$hash_md5_conjunto_paginas != duplicados$hash_conjunto_representante |")
        return text

    c.patch_function("monitora_relatorios_analiticos_gerar", gerar)
    c.patch_function("monitora_relatorios_analiticos_renderizar", lambda text: helpers.unique(text,
        "  conteudo <- monitora_v306_limpar_notas(conteudo, dir_relatorio)",
        "  conteudo <- monitora_v308_expandir_figuras(conteudo, dir_relatorio)\n"
        "  conteudo <- monitora_v306_limpar_notas(conteudo, dir_relatorio)"))
    # O organizador tambem roda antes de carregar o modulo analitico. Portanto
    # esta protecao de unidades autocontidas usa somente funcoes R base inline.
    c.patch_function("monitora_output_reescrever_referencias", lambda text: helpers.unique(text,
        "  candidatos <- monitora_v307_caminhos_candidatos(candidatos, output_dir)",
        "  candidatos <- monitora_v307_caminhos_candidatos(candidatos, output_dir)\n"
        '  raiz_analises_paginadas <- file.path(output_dir, "08_analises")\n'
        "  if (dir.exists(raiz_analises_paginadas)) {\n"
        "    unidades <- list.dirs(raiz_analises_paginadas, recursive = FALSE, full.names = TRUE)\n"
        "    assinadas <- vapply(unidades, function(p) length(list.files(\n"
        '      file.path(p, "figuras"), pattern = "^ev_[a-f0-9]{16}_paginas[.]csv$",\n'
        "      recursive = FALSE)) > 0L, logical(1L))\n"
        '    prefixos <- paste0(chartr("\\\\", "/", unidades[assinadas]), "/")\n'
        "    if (length(prefixos)) candidatos <- candidatos[!vapply(\n"
        '      chartr("\\\\", "/", candidatos), function(p) any(startsWith(p, prefixos)), logical(1L))]\n'
        "  }"))
