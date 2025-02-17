# xPDF.py
import sys
import lhapdf


def get_pdf_values(x, Q, id):
    """
        Retrieve the Parton Distribution Function (PDF) value for a given parton at a specific x and Q.
        Parameters:
    -----------
    x : float
        (0 < x < 1).
    Q : float
        The energy scale in GeV.
    id : int
        The PDG ID of the parton (e.g., 1 for down quark, -1 for anti-down, etc.).

    Returns:
    --------
    float
        The PDF value x*f(x, Q), where f(x, Q) is the parton distribution function.

    Example:
    --------
    get_pdf_values(0.1, 125, 1)
    0.00342
    """
    dfset = "NNPDF30_nnlo_as_0118"
    pdf = lhapdf.mkPDF(dfset, 0)
    return pdf.xfxQ(id, x, Q)



def graphics():
    """
    Generate and display two log-log scale plots of the PDF.
    
    This function computes and plots:
    - `pdf_values`: PDF values normalized by x.
    - `pdf_values2`: PDF values without normalization.

    The function uses a fixed Q = 125 GeV and PDG ID = 1 (up quark).
    
    Example:
    --------
    >>> graphics()
    (Displays a plot of the PDF data.)
    """
    import numpy as np
    import matplotlib.pyplot as plt
    
    print("Test graph.")
    x_values = np.logspace(-5, 0, 50)
    pdf_values = [get_pdf_values(x, 125, 1) / x for x in x_values]
    pdf_values2 = [get_pdf_values(x, 125, 1) for x in x_values]

    fig, ax = plt.subplots(figsize=(8, 6))

    ax.plot(x_values, pdf_values, 'b.-', label='PDF / x')
    ax.plot(x_values, pdf_values2, 'r.-', label='PDF')
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.grid(True, which='both', linestyle='--', linewidth=0.7)
    ax.legend()
    
    plt.xlabel("x")
    plt.ylabel("PDF value")
    plt.title("PDF Visualization")
    plt.show()



if __name__ == "__main__":
    # Command-line interface for retrieving PDF values from Wolfram code.

    # Usage:
    # ------
    # python script.py <x> <Q> <id>
    #
    # Arguments:
    # ----------
    # x  : float  -> Momentum fraction of parton.
    # Q  : float  -> Energy scale in GeV.
    # id : int    -> Parton ID (based on PDG numbering).
    #
    # Example:
    # --------
    # python script.py 0.1 125 1
    lhapdf.setVerbosity(0)
    x = float(sys.argv[1])
    Q = float(sys.argv[2])
    id = int(sys.argv[3])
    print(get_pdf_values(x, Q, id))
