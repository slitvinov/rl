# Install

## dpkg
    wget https://mr.gy/blog/clozure-cl_1.11_amd64.deb
    sudo apt purge sbcl slime && sudo apt autoremove
    dpkg -i clozure-cl_1.11_amd64.deb

## pacman

    git clone https://aur.archlinux.org/ccl.git
    cd ccl
    makepkg

# References

- A Debian Package for Clozure Common Lisp https://mr.gy/blog/clozure-cl-deb.html
- code http://incompleteideas.net/sutton/book/code/code.html
