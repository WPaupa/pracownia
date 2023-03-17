import pandas as pd

def texify(filepath, name, **args):
    csv = pd.read_csv(filepath, **args)
    result = """\\begin{table}[!htbp]\\centering
\\begin{tabular}{"""
    colnames = list(filter(lambda x: not x.startswith('Delta'), csv.columns))
    result += '|'.join(['c'] * len(colnames)) + "}\n"
    result += '&'.join(colnames) + " \\\\\\hline\n"
    for row in csv.iterrows():
        result += '&'.join(map(lambda x: str(x[1]) + (' \pm ' + str(row[1]['Delta' + x[0]]) if 'Delta' + x[0] in csv.columns else ''), filter(lambda x: not x[0].startswith('Delta'), dict(row[1]).items()))) + '\\\\\n'
    result += """\\end{tabular}
\\caption{""" + name + """}
\\end{table}~\\\\
"""
    print(result)