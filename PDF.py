import numpy as np
import lhapdf

    
# Загрузка нужного набора данных PDF

def PDF(id,q,x):
    df_set = "NNPDF30_nnlo_as_0118"
    pdf = lhapdf.mkPDF(df_set, 0)
    return pdf.xfxQ(id, x, q)