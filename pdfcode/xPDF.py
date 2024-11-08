# xPDF.py
import sys
import lhapdf


def get_pdf_values(x, Q, id):
    df_set = "NNPDF30_nnlo_as_0118"
    pdf = lhapdf.mkPDF(df_set, 0)
    return pdf.xfxQ(id, x, Q)

lhapdf.setVerbosity(0)
x = float(sys.argv[1])
Q = float(sys.argv[2])
id = int(sys.argv[3])

print(get_pdf_values(x, Q, id))

