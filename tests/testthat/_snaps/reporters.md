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
      [][install] rev.both.dependency finished ()
      [][install] pkg.ok.error started
      [][install] pkg.ok.error finished ()
      [][install] pkg.ok.error started
      [][check] rev.both.ok started
      [][install] pkg.ok.error finished ()
      [][check] rev.both.ok finished with 1 NOTE ()
      [][check] rev.both.error started
      [][check] rev.both.error finished with 1 NOTE ()
      [][check] rev.both.ok started
      [][check] rev.both.ok finished with 1 NOTE ()
      [][check] rev.both.error started
      [][check] rev.both.error finished with 1 ERROR, 1 WARNING ()
      Finished in 

