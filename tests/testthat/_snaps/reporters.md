# reporter_basic_tty works as expected for pkg.none

    Code
      run(design, reporter = reporter)
    Message

# reporter_basic_tty works as expected for pkg.ok.error

    Code
      run(design, reporter = reporter)
    Message
      <checked> Checks
      [][install] rev.both.dependency started
      [][install] rev.both.dependency finished [1/7] ()
      [][install] pkg.ok.error started
      [][install] pkg.ok.error finished [2/7] ()
      [][install] pkg.ok.error started
      [][check] rev.both.ok started
      [][install] pkg.ok.error finished [3/7] ()
      [][check] rev.both.ok finished with 1 NOTE [4/7] ()
      [][check] rev.both.error started
      [][check] rev.both.error finished with 1 NOTE [5/7] ()
      [][check] rev.both.ok started
      [][check] rev.both.ok finished with 1 NOTE [6/7] ()
      [][check] rev.both.error started
      [][check] rev.both.error finished with 1 ERROR, 1 WARNING [7/7] ()
      Finished in 

