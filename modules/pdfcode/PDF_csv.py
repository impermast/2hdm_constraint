import numpy as np
import matplotlib.pyplot as plt
import lhapdf
import csv
import os

def pdf_csv(id: int, q: float, output_folder: str = "/root/generated_pdf") -> str:
    """
    Generate a CSV file containing PDF values for a given parton ID and energy scale Q.

    Parameters:
    -----------
    id : int
        The PDG ID of the parton (e.g., 21 for gluon, 1 for down quark, etc.).
    q : float
        The factorization scale in GeV.
    output_folder : str, optional
        The directory where the CSV file will be saved (default: "/root/generated_pdf").

    Returns:
    --------
    str
        The path to the generated CSV file.

    Example:
    --------
    >>> pdf_csv(21, 91)
    '/root/generated_pdf/pdf_21_91.csv'
    """
    global df_set

    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    pdf = lhapdf.mkPDF(df_set, 0)
    x_values = np.logspace(-6, 0, 1000)
    pdf_values = [pdf.xfxQ(id, x, q) for x in x_values]

    filename = f"pdf_{id}_{q}.csv"
    filepath = os.path.join(output_folder, filename)

    with open(filepath, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['x', 'PDF'])
        for x, pdf_val in zip(x_values, pdf_values):
            writer.writerow([x, pdf_val])

    return filepath

def get_pdf_table(id_list: list, x_values: np.ndarray, q: float) -> list:
    """
    Generate a table of PDF values for multiple parton IDs over a range of x values.

    Parameters:
    -----------
    id_list : list of int
        A list of PDG IDs for the partons.
    x_values : np.ndarray
        An array of x values for which PDFs are evaluated.
    q : float
        The factorization scale in GeV.

    Returns:
    --------
    list
        A nested list where the first row contains column headers, and subsequent rows contain x values and PDFs.

    Example:
    --------
    >>> x_vals = np.logspace(-6, 0, 100)
    >>> get_pdf_table([-5, -4, -3, -2, -1, 1, 2, 3, 4, 5], x_vals, 91)
    [['x', 'PDF_-5', 'PDF_-4', ..., 'PDF_5'], [...], [...], ...]
    """
    global df_set

    pdf = lhapdf.mkPDF(df_set, 0)
    pdf_table = [['x'] + [f'PDF_{id}' for id in id_list]]

    for x in x_values:
        row = [x] + [pdf.xfxQ(id, x, q) for id in id_list]
        pdf_table.append(row)

    return pdf_table

def write_pdf_table_to_csv(filename: str, pdf_table: list):
    """
    Write a PDF table to a CSV file.

    Parameters:
    -----------
    filename : str
        The name of the output CSV file.
    pdf_table : list
        A nested list containing x values and corresponding PDF values.

    Returns:
    --------
    None

    Example:
    --------
    >>> write_pdf_table_to_csv("output.csv", get_pdf_table([...], [...], 91))
    (Creates a CSV file with PDF data.)
    """
    with open(filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(pdf_table[0])

        for row in pdf_table[1:]:
            writer.writerow(row)

def testgraph():
    """
    Generate a plot of PDF values for a given parton at different energy scales.

    The function:
    - Loads the PDF set.
    - Plots PDF values for a parton at Q = 125 GeV.
    - Plots PDF values for the same parton at Q = 100 * 125 GeV.
    - Computes and visualizes the absolute difference between the two PDFs.

    Example:
    --------
    >>> testgraph()
    (Displays two subplots with PDF data.)
    """
    global df_set

    pdf = lhapdf.mkPDF(df_set, 0)

    mz = 91.1876
    mh = 125
    q = mh
    x_values = np.logspace(-5, -0.2, 1000)

    pdf_values = [pdf.xfxQ(5, x, q) for x in x_values]
    pdf_values2 = [pdf.xfxQ(5, x, 100 * q) for x in x_values]
    diff = [abs(pdf_values[i] - pdf_values2[i]) for i in range(len(pdf_values2))]

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 6))

    ax1.plot(x_values, pdf_values, 'b.-', label='PDF 1')
    ax1.plot(x_values, pdf_values2, 'r-', label='PDF 2')
    ax1.set_xscale('log')
    ax1.set_yscale('log')
    ax1.grid(True, which='both', linestyle='--', linewidth=0.7)
    ax1.legend()

    ax2.plot(x_values, diff, 'g-', label='Difference')
    ax2.set_xscale('log')
    ax2.set_yscale('log')
    ax2.grid(True, which='both', linestyle='--', linewidth=0.7)
    ax2.legend()

    plt.tight_layout()
    plt.show()
    plt.close()

if __name__ == "__main__":
    global df_set
    df_set = "NNPDF30_nnlo_as_0118"
    q = 91
    id_list = [-5,-4,-3,-2,-1,1,2,3,4,5]
    x_values = np.logspace(-6, 0, 1000)  # Диапазон от очень малых значений x до 1

    # print("\n\n q = ", q," PDF = ", df_set,"\n\n")

    # pdf_table = get_pdf_table(id_list,x_values,q)
    # filename = f"/root/output/output_{q}.csv"
    # write_pdf_table_to_csv(filename, pdf_table)
    testgraph()