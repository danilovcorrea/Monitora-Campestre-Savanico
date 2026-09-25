# Monitora Campestre-Savânico — v3.0.4

Build: `v3.0.4-20260925-r01`.

Este pacote contém o [script autônomo](monitora_campsav_alvo_global_v3.0.4.R),
[guia operacional](GUIA_USUARIO_v3.0.4.md), [notas da versão](RELEASE_NOTES_v3.0.4.md)
e manual em [HTML](manual_usuario/manual_usuario_v3.0.4.html) e
[PDF](manual_usuario/manual_usuario_v3.0.4.pdf). As somas de verificação estão em
[SHA256SUMS.txt](SHA256SUMS.txt). Não contém bases institucionais.

Extraia os arquivos em uma pasta local curta. Para executar uma rodada, copie o
R para uma pasta exclusiva com `input/`, escolha o modo adequado no bloco inicial
e use Source no RStudio. O arquivo permanece abaixo de 5.000.000 bytes inclusive
em CRLF. Consulte o manual antes de editar opções ou iniciar a curadoria.

A geração QField e a importação de camadas estão ligadas por padrão. Fontes
externas ficam em `qfield_input/`, fora do input biológico. Bloqueios do projeto e
ausência de fontes válidas são informados no console; os demais produtos seguem.
Confira o projeto no aparelho, inclusive offline, antes do uso em campo.

Software e documentação: https://github.com/danilovcorrea/Monitora-Campestre-Savanico
