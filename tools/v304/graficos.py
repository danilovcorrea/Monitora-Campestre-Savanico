# Correções exclusivamente geométricas: nenhuma estatística é recalculada.
name = 'monitora_stat_adicionar_simbolo_composicao_borda'
a = s.index(name + ' <- function(')
b = s.index('\n}\n', a) + 3
p = s[a:b]
old = '    x_min_escala <- if (length(lims_escala)) min(lims_escala) else 0'
assert p.count(old) == 1
p = p.replace(old, '''    # O corredor do eixo contém ANO/esforço em x negativo. A escala deve
    # preservá-lo antes do recorte por coord_cartesian; caso contrário os
    # rótulos são descartados mesmo estando dentro do painel final.
    corredor_x <- suppressWarnings(as.numeric(plot_out$coordinates$limits$x))
    corredor_x <- corredor_x[is.finite(corredor_x)]
    x_min_escala <- min(c(if (length(lims_escala)) min(lims_escala) else 0, corredor_x))''')
old = '    x_max_escala <- if (length(lims_escala)) max(lims_escala) else x_necessario'
assert p.count(old) == 1
# O máximo também deve contemplar anos sem símbolo composicional: o maior
# rótulo/IC pode pertencer justamente a um ano excluído da camada de símbolos.
p = p.replace(old, '    x_max_escala <- max(c(if (length(lims_escala)) max(lims_escala) else x_necessario, corredor_x))')
s = s[:a] + p + s[b:]
name = 'monitora_plot_preparar_rotulos_proporcao_obrigatorios'
a = s.index(name + ' <- function(')
b = s.index('\n}\n', a) + 3
p = s[a:b]
assert p.count('n_bloco == 2L, 0.13,') == 1
# Duas caixas no mesmo lado/ano precisam de espaço para as duas linhas
# numéricas e a linha de símbolos acrescentada na exportação. Permanecem
# centradas no ano e com suas guias ligadas aos mesmos segmentos.
p = p.replace('n_bloco == 2L, 0.13,', 'n_bloco == 2L, 0.23,')
s = s[:a] + p + s[b:]
