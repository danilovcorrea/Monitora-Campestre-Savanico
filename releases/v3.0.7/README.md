# Monitora Campestre-Savânico — v3.0.7

Build: `v3.0.7-20261112-r01`.

Este pacote contém o [script autônomo](monitora_campsav_alvo_global_v3.0.7.R),
[guia operacional](GUIA_USUARIO_v3.0.7.md), [notas da versão](RELEASE_NOTES_v3.0.7.md)
e manual em [HTML](manual_usuario/manual_usuario_v3.0.7.html) e
[PDF](manual_usuario/manual_usuario_v3.0.7.pdf). Confira as somas em
[SHA256SUMS.txt](SHA256SUMS.txt). Não contém bases, relatórios ou logs de UCs.

Extraia em uma pasta local curta. Copie o R para uma pasta exclusiva com
`input/`, revise o modo e as opções e use **Source** no RStudio. O arquivo fica
abaixo de 5.000.000 bytes inclusive em CRLF. Consulte o manual antes da curadoria.

Para continuar uma rodada, transporte juntos `registros_corrig.csv` e toda a
pasta de linhagem da saída anterior. Ocorrências com justificativa vigente
continuam disponíveis para consulta e deixam de ser oferecidas para novo
registro. Justificar não resolve uma impeditiva. Histórico legado ausente
exige recuperação auditável; cache incompatível deve ser regenerado.

Os relatórios analíticos em Word usam Arial e paleta verde, com conteúdo científico preservado. Atualize
campos e índice com **Ctrl+A, F9** e confira a paginação. A validação desta versão
se limita ao escopo aprovado; modos não executados e avisos de caminho das
entregas institucionais estão descritos nas notas.

Geração QField e importação de camadas estão ligadas por padrão. Fontes
externas entram em `qfield_input/`, fora do input biológico. Confira os bloqueios
e teste o projeto no aparelho, inclusive offline, antes do uso em campo.

Software e documentação: https://github.com/danilovcorrea/Monitora-Campestre-Savanico
