# nagios-plugin-retcond

Nagios plugin to monitor retcond

# Usage

This plugin can be run either on the Nagios (or compatible) monitoring
server, or on the monitored host via NRPE.

## Example

```
$ check_retcond -e http://retcon.example.com:8888
OK: perfdata only | notifications=2394562348956.0;;;; entity_aleph_notifications=23.0;;;; entity_aleph_creates=0c;;;; entity_aleph_updates=923457c;;;; entity_aleph_deletes=2345c;;;; entity_aleph_conflicts=2.0;;;; entity_aleph_internal_keys=1234.0;;;; source_aleph_beth_notifications=235.0;;;; source_aleph_beth_foreign_keys=567.0;;;; source_aleph_gimel_notifications=2345.0;;;; source_aleph_gimel_foreign_keys=235123.0;;;;
$ echo $?
0
```
