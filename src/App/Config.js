exports.config = {
  title: 'Pux Starter App',
  public_path: process.env.NODE_ENV === 'production'
               ? '/visa-check/'
        : '/static/dist'
}
