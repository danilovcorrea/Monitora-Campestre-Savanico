# Monitora Campestre-Savânico v2.9.18

Publicada em 26 de agosto de 2026 sobre a v2.9.17.

## Compatibilidade com o RStudio no Windows

- O arquivo único foi reduzido de 5.212.182 para 4.465.796 bytes em LF.
- Mesmo após conversão integral para CRLF, ocupa 4.549.339 bytes: margem de
  693.541 bytes sob o limite rígido de 5 MiB do editor do RStudio.
- O alerta de confirmação para arquivo grande pode continuar aparecendo por
  estar acima do limite configurável de 2 MiB. Confirme a abertura e clique em
  **Source**; o arquivo não deve mais ser recusado como grande demais.
- O repositório passa a fixar LF nos scripts `.R`, evitando crescimento
  automático em clones realizados no Windows.

## Contrato único autossuficiente

- As tabelas consolidadas dos XLSForms continuam no próprio arquivo `.R`.
- A representação física passa a usar serialização R versão 2, XDR e gzip,
  restaurada em memória com funções do R base.
- O gate confirmou igualdade integral de nomes, ordem, colunas e valores das
  416 linhas de campos, 1.349 opções, 465 dependências e quatro arquivos.
- Nenhum XLSForm ou arquivo auxiliar local é necessário para executar o script.

## Homologação

- Parse e cópia CRLF real aprovados.
- Execução real com R 4.6 no Windows em 47.773 registros: 94,8 s, ante 97,9 s
  da v2.9.17 no mesmo conjunto.
- Resultado semântico idêntico: 47.773 linhas, 135 colunas e zero célula
  divergente, descontados apenas metadados técnicos da execução.
- Fechamento contratual, relatórios editáveis e esquemas reais de APAI, PNSC,
  PNCV, PNM, RBG e RVSVOB aprovados.

## Preservação

- O bloco funcional de inicialização rápida permanece equivalente ao da
  v2.9.17, descontados somente versão e build.
- Leitura, painel, replay, linhagem, correções, produtos, estatísticas,
  relatórios e cartografia não foram alterados.
- A remoção de comentários internos redundantes reduz apenas tamanho físico e
  tempo de leitura do editor; cabeçalho, variáveis e seções permanecem claros.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.9.18.R`. O build exibido no console é
`v2.9.18-20260826-r01`.
