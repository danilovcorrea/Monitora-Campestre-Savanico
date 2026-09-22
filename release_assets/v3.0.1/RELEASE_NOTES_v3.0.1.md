# v3.0.1 — destinos espaciais e projetos QField

Ao inspecionar uma coleta no painel, a lista de UAs de destino podia permanecer
presa ao foco lateral. A lista agora acompanha os filtros gerais e espaciais,
conserva destinos válidos e remove seleções incompatíveis. Coleta, ano e lote
usam o mesmo domínio; operações parcialmente fora dele são recusadas por inteiro.

As camadas anuais QField exibem `verg_ini_YYYY` e `verg_fin_YYYY`, para facilitar
a leitura em telas pequenas. Nomes físicos, fontes, estilos e coordenadas são
preservados. Geração do projeto e importação de camadas passam ao padrão S.
Ausência de fontes válidas e impedimentos do projeto são registrados no console
e nas auditorias, sem interromper os demais produtos ou liberar bloqueios reais.

Corrige também a organização final das pastas de diagnósticos opcionais de
contrato e pipes. A migração preserva hashes, trata conflitos sem sobrescrita e
mantém o bloqueio para pastas não reconhecidas. Manual e guia foram atualizados.

## Compatibilidade

O contrato embutido de 129 campos, a interpretação dos dados, os motores de
cálculo e os critérios espaciais permanecem preservados. O R continua autônomo e
abaixo de 5.000.000 bytes em LF e CRLF. O pacote contém somente software e
documentação; bases e produtos institucionais não integram a distribuição.

A geração do pacote QField não substitui a conferência de navegação no aparelho
antes do uso em campo. Pendências dos dados continuam exigindo curadoria conforme
o contrato; não são eliminadas por esta atualização.


## Verificação e pendências de homologação

Publicação autorizada com as pendências operacionais abaixo explicitadas.
Passaram os testes de sintaxe, contrato de 129 campos, tamanho LF/CRLF, domínio
dos destinos, falhas QField não fatais e organização de auditorias no R Linux e
no R Windows. O painel completo em Chrome/Linux passou nos cenários de filtros,
prévia, descarte, salvamento e reabertura incremental. A persistência alterou
somente as coordenadas previstas na cópia descartável de teste.

- **RStudio Windows:** permanece pendente o ensaio visual/operacional por Source,
  navegação, filtros e encerramento/reabertura dentro do RStudio. Testes Rscript
  Windows e Chrome/Linux não substituem essa verificação. A ferramenta de UI
  desta sessão ficou indisponível por incompatibilidade do diretório WSL.
- **QField em aparelho:** permanece pendente a importação e a navegação móvel,
  incluindo modo avião. Foram verificados nomes, estilos, arquivos, GeoPackage,
  cobertura offline e integridade do ZIP; isso não comprova o uso no aparelho.

O manual foi gerado no Windows e conferido. O R tem 4.844.428 bytes em LF e
4.928.342 bytes em CRLF, ambos inferiores ao teto obrigatório de 5.000.000 bytes.
As funções de dados e análise permanecem iguais às da candidata testada; a
pré-publicação alterou apenas versão e build. Nenhuma base institucional é
publicada. Não se declara homologação operacional integral enquanto os ensaios
pendentes não forem registrados.
