call AddCycleGroup('global', [ 'return', 'break', 'continue' ])
call AddCycleGroup('global', [ 'staging', 'production' ])
call AddCycleGroup('global', [ 'debit', 'credit' ])

call AddCycleGroup(
  \ [ 'ruby', 'eruby', 'haml', 'slim' ],
  \ [ 'return', 'break', 'continue', 'next', 'retry' ])

call AddCycleGroup(
  \ [ 'javascript', 'coffee' ],
  \ [ 'addClass', 'removeClass' ])

call AddCycleGroup('sh', [ 'if', 'elif', 'else', 'fi' ])
call AddCycleGroup('sh', [ 'do', 'then' ])
