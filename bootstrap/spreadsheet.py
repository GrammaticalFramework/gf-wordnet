import xlsxwriter

# Create a workbook and add a worksheet.
workbook = xlsxwriter.Workbook('data/predictions.xlsx')
worksheet = workbook.add_worksheet()

# Some data we want to write to the worksheet.
expenses = (
    ['Rent', 1000],
    ['Gas',   100],
    ['Food',  300],
    ['Gym',    50],
)

# Start from the first cell. Rows and columns are zero indexed.

with open("data/predictions.tsv") as f:
    data = [line.split('\t') for line in f]

langs = sorted(set(lang for id,lang,w,inf in data))

bold      = workbook.add_format({'bold': True})
checked   = workbook.add_format({'bg_color': 'green'})
unchecked = workbook.add_format({'bg_color': 'yellow'})
guessed   = workbook.add_format({'bg_color': 'red'})

row = 0
for col,lang in enumerate(langs):
    worksheet.write(row, col+1, lang, bold)

last_id = None
for id,lang,lemma,inf in data:
    if id != last_id:
        last_id = id
        row += 1

    (o,s,w,l,c,d) = eval(inf)
    fmt = None
    if s == 0:
        fmt = checked
    elif s == 1 and l > 1:
        fmt = checked
    elif w == 0 and l > 1:
        fmt = checked
    elif w == 0 and s == 1:
        fmt = checked
    elif s == 1:
        fmt = unchecked
    else:
        fmt = guessed

    worksheet.write(row, 0, id, bold)
    worksheet.write(row, langs.index(lang)+1, lemma, fmt)

# Iterate over the data and write it out row by row.
#for item, cost in (expenses):
 #   worksheet.write(row, col,     item)
  #  worksheet.write(row, col + 1, cost)
   # row += 1

# Write a total using a formula.
#worksheet.write(row, 0, 'Total')
#worksheet.write(row, 1, '=SUM(B1:B4)')

workbook.close()
