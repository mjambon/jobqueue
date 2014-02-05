let encode s = Nlencoding.Url.encode ~plus:false  s
let decode s = Nlencoding.Url.decode ~plus:false s
