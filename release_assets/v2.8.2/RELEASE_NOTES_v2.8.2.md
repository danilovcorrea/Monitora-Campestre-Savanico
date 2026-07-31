# v2.8.2 — Planilha para inclusão de registros novos no SISMONITORA

A versão `v2.8.2` adiciona, de forma opcional e isolada, uma planilha XLSX para
importação de registros novos no SISMONITORA. O produto é derivado
exclusivamente de `registros_validados.csv` aprovado e gerado na mesma execução.

## Produto novo

- Nome: `registros_validados_importacao_sismonitora.xlsx`.
- Modelo de referência: XLSForm/template 21FEV25.
- Abas preservadas: `Preenchimento`, `Opções válidas` e `Campos Comuns`.
- A aba `Preenchimento` contém 115 colunas, incluindo `uc` na segunda posição.
- Campos comuns aparecem somente na primeira linha de cada bloco de 101 pontos.
- O produto aceita somente um contexto de UC, ciclo e campanha por arquivo.

## Identidade e modo de inclusão

A planilha destina-se exclusivamente à inclusão de registros novos,
originalmente coletados em formulário de papel.

- As colunas `uuid` e `amostragem/registro/uuid` permanecem no schema exigido.
- Todas as células de dados dessas duas colunas ficam vazias para que o
  SISMONITORA atribua novas identidades.
- Os UUIDs existentes em `registros_validados.csv` permanecem intocados.
- A auditoria registra a linhagem e as contagens dos identificadores omitidos
  somente do produto de transporte.
- A planilha não deve ser utilizada para editar registros já cadastrados.

Essa regra corrige a rejeição `UUID inválido` observada quando valores
provenientes de uma exportação do SISMONITORA eram reenviados como se
representassem novos cadastros.

## Compatibilidade e desempenho

- A opção
  `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS_IMPORTACAO_SISMONITORA`
  permanece em `N` por padrão.
- Com a opção desligada, o módulo não é materializado, não lê dados e não
  carrega dependências adicionais.
- A geração exige
  `MONITORA_OPCAO_GERAR_REGISTROS_VALIDADOS = "S"` e bloqueia o uso de um
  `registros_validados.csv` preexistente.
- Interface, painel, atributos exibidos, operações semânticas, 13 modos
  públicos, contrato único, replay, produtos centrais, validação espacial e
  KML/KMZ permanecem inalterados.

Devido a uma limitação comprovada do importador, `observacoes_gerais` é omitido
somente da planilha enquanto a função XPath `regex` do XLSForm 21FEV25 não for
avaliada. O valor original permanece na fonte canônica e na auditoria.

## Validação

- Campanha FNCS 2026: 58 coletas e 5.858 registros.
- Aba `Preenchimento`: 5.858 linhas de dados, 115 colunas e dois cabeçalhos.
- Três abas preservadas na ordem original.
- UUID raiz preenchido na planilha: zero.
- UUID de registro preenchido na planilha: zero.
- Componentes congelados do modelo permaneceram byte a byte idênticos.
- Campos comuns, ordem dos pontos 1–101, CPF textual e opções contextuais foram
  verificados.
- Nenhum erro de fórmula foi encontrado na inspeção independente do XLSX.
- Tempo focal de geração dos bytes finais: 3,625 s.
- Regressão histórica de CPF, UUID e fechamento hierárquico aprovada.

## Arquivo principal

Use `monitora_campsav_alvo_global_v2.8.2.R`. O build declarado no console é
`v2.8.2-20260730.1`.
