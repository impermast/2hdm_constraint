# xPDF.py
import sys
import lhapdf


def get_pdf_values(x, Q, id):
    df_set = "NNPDF30_nnlo_as_0118"
    pdf = lhapdf.mkPDF(df_set, 0)
    return pdf.xfxQ(id, x, Q)



def graphics():
    import numpy as np
    import matplotlib.pyplot as plt
    print("Test graph.")
    x_values = np.logspace(-5,0,50)
    pdf_values = [get_pdf_values(x, 125,1)/x for x in x_values] 
    pdf_values2 = [get_pdf_values(x, 125,1) for x in x_values] 
        # Построение графика
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 6))

    # Построение графиков
    ax1.plot(x_values, pdf_values, 'b.-', label='PDF')
    ax1.plot(x_values, pdf_values2, 'r.-', label='x*PDF') 
    ax1.set_xscale('log')  
    ax1.set_yscale('log') 
    ax1.grid(True, which='both', linestyle='--', linewidth=0.7)
    ax1.legend()  
    plt.show()


if __name__ == "__main__":
    lhapdf.setVerbosity(0)
    x = float(sys.argv[1])
    Q = float(sys.argv[2])
    id = int(sys.argv[3])
    print(get_pdf_values(x, Q, id))