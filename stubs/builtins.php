function zend_version(): string {}
function func_num_args(): int {}
function func_get_arg(int $position): mixed {}
function func_get_args(): array {}
function strlen(string $string): int {}
function strcmp(string $string1, string $string2): int {}
function strncmp(string $string1, string $string2, int $length): int {}
function strcasecmp(string $string1, string $string2): int {}
function strncasecmp(string $string1, string $string2, int $length): int {}
function error_reporting(?int $error_level = NULL): int {}
function define(string $constant_name, mixed $value, bool $case_insensitive = false): bool {}
function defined(string $constant_name): bool {}
function get_class(object $object): string {}
function get_called_class(): string {}
function get_parent_class(object|string $object_or_class): string|false {}
function is_subclass_of(mixed $object_or_class, string $class, bool $allow_string = true): bool {}
function is_a(mixed $object_or_class, string $class, bool $allow_string = false): bool {}
function get_class_vars(string $class): array {}
function get_object_vars(object $object): array {}
function get_mangled_object_vars(object $object): array {}
function get_class_methods(object|string $object_or_class): array {}
function method_exists($object_or_class, string $method): bool {}
function property_exists($object_or_class, string $property): bool {}
function class_exists(string $class, bool $autoload = true): bool {}
function interface_exists(string $interface, bool $autoload = true): bool {}
function trait_exists(string $trait, bool $autoload = true): bool {}
function enum_exists(string $enum, bool $autoload = true): bool {}
function function_exists(string $function): bool {}
function class_alias(string $class, string $alias, bool $autoload = true): bool {}
function get_included_files(): array {}
function get_required_files(): array {}
function trigger_error(string $message, int $error_level = 1024): bool {}
function user_error(string $message, int $error_level = 1024): bool {}
function set_error_handler(?callable $callback, int $error_levels = 32767) {}
function restore_error_handler(): true {}
function set_exception_handler(?callable $callback) {}
function restore_exception_handler(): true {}
function get_declared_classes(): array {}
function get_declared_traits(): array {}
function get_declared_interfaces(): array {}
function get_defined_functions(bool $exclude_disabled = true): array {}
function get_defined_vars(): array {}
function get_resource_type($resource): string {}
function get_resource_id($resource): int {}
function get_resources(?string $type = NULL): array {}
function get_loaded_extensions(bool $zend_extensions = false): array {}
function get_defined_constants(bool $categorize = false): array {}
function debug_backtrace(int $options = 1, int $limit = 0): array {}
function debug_print_backtrace(int $options = 0, int $limit = 0): void {}
function extension_loaded(string $extension): bool {}
function get_extension_funcs(string $extension): array|false {}
function gc_mem_caches(): int {}
function gc_collect_cycles(): int {}
function gc_enabled(): bool {}
function gc_enable(): void {}
function gc_disable(): void {}
function gc_status(): array {}
function strtotime(string $datetime, ?int $baseTimestamp = NULL): int|false {}
function date(string $format, ?int $timestamp = NULL): string {}
function idate(string $format, ?int $timestamp = NULL): int|false {}
function gmdate(string $format, ?int $timestamp = NULL): string {}
function mktime(int $hour, ?int $minute = NULL, ?int $second = NULL, ?int $month = NULL, ?int $day = NULL, ?int $year = NULL): int|false {}
function gmmktime(int $hour, ?int $minute = NULL, ?int $second = NULL, ?int $month = NULL, ?int $day = NULL, ?int $year = NULL): int|false {}
function checkdate(int $month, int $day, int $year): bool {}
function strftime(string $format, ?int $timestamp = NULL): string|false {}
function gmstrftime(string $format, ?int $timestamp = NULL): string|false {}
function time(): int {}
function localtime(?int $timestamp = NULL, bool $associative = false): array {}
function getdate(?int $timestamp = NULL): array {}
function date_create(string $datetime = 'now', ?DateTimeZone $timezone = NULL): DateTime|false {}
function date_create_immutable(string $datetime = 'now', ?DateTimeZone $timezone = NULL): DateTimeImmutable|false {}
function date_create_from_format(string $format, string $datetime, ?DateTimeZone $timezone = NULL): DateTime|false {}
function date_create_immutable_from_format(string $format, string $datetime, ?DateTimeZone $timezone = NULL): DateTimeImmutable|false {}
function date_parse(string $datetime): array {}
function date_parse_from_format(string $format, string $datetime): array {}
function date_get_last_errors(): array|false {}
function date_format(DateTimeInterface $object, string $format): string {}
function date_modify(DateTime $object, string $modifier): DateTime|false {}
function date_add(DateTime $object, DateInterval $interval): DateTime {}
function date_sub(DateTime $object, DateInterval $interval): DateTime {}
function date_timezone_get(DateTimeInterface $object): DateTimeZone|false {}
function date_timezone_set(DateTime $object, DateTimeZone $timezone): DateTime {}
function date_offset_get(DateTimeInterface $object): int {}
function date_diff(DateTimeInterface $baseObject, DateTimeInterface $targetObject, bool $absolute = false): DateInterval {}
function date_time_set(DateTime $object, int $hour, int $minute, int $second = 0, int $microsecond = 0): DateTime {}
function date_date_set(DateTime $object, int $year, int $month, int $day): DateTime {}
function date_isodate_set(DateTime $object, int $year, int $week, int $dayOfWeek = 1): DateTime {}
function date_timestamp_set(DateTime $object, int $timestamp): DateTime {}
function date_timestamp_get(DateTimeInterface $object): int {}
function timezone_open(string $timezone): DateTimeZone|false {}
function timezone_name_get(DateTimeZone $object): string {}
function timezone_name_from_abbr(string $abbr, int $utcOffset = -1, int $isDST = -1): string|false {}
function timezone_offset_get(DateTimeZone $object, DateTimeInterface $datetime): int {}
function timezone_transitions_get(DateTimeZone $object, int $timestampBegin = -9223372036854775807-1, int $timestampEnd = 9223372036854775807): array|false {}
function timezone_location_get(DateTimeZone $object): array|false {}
function timezone_identifiers_list(int $timezoneGroup = 2047, ?string $countryCode = NULL): array {}
function timezone_abbreviations_list(): array {}
function timezone_version_get(): string {}
function date_interval_create_from_date_string(string $datetime): DateInterval|false {}
function date_interval_format(DateInterval $object, string $format): string {}
function date_default_timezone_set(string $timezoneId): bool {}
function date_default_timezone_get(): string {}
function date_sunrise(int $timestamp, int $returnFormat = 1, ?float $latitude = NULL, ?float $longitude = NULL, ?float $zenith = NULL, ?float $utcOffset = NULL): string|int|float|false {}
function date_sunset(int $timestamp, int $returnFormat = 1, ?float $latitude = NULL, ?float $longitude = NULL, ?float $zenith = NULL, ?float $utcOffset = NULL): string|int|float|false {}
function date_sun_info(int $timestamp, float $latitude, float $longitude): array {}
function libxml_set_streams_context($context): void {}
function libxml_use_internal_errors(?bool $use_errors = NULL): bool {}
function libxml_get_last_error(): LibXMLError|false {}
function libxml_get_errors(): array {}
function libxml_clear_errors(): void {}
function libxml_disable_entity_loader(bool $disable = true): bool {}
function libxml_set_external_entity_loader(?callable $resolver_function): bool {}
function libxml_get_external_entity_loader(): ?callable {}
function openssl_x509_export_to_file(OpenSSLCertificate|string $certificate, string $output_filename, bool $no_text = true): bool {}
function openssl_x509_export(OpenSSLCertificate|string $certificate, $output, bool $no_text = true): bool {}
function openssl_x509_fingerprint(OpenSSLCertificate|string $certificate, string $digest_algo = 'sha1', bool $binary = false): string|false {}
function openssl_x509_check_private_key(OpenSSLCertificate|string $certificate, $private_key): bool {}
function openssl_x509_verify(OpenSSLCertificate|string $certificate, $public_key): int {}
function openssl_x509_parse(OpenSSLCertificate|string $certificate, bool $short_names = true): array|false {}
function openssl_x509_checkpurpose(OpenSSLCertificate|string $certificate, int $purpose, array $ca_info = array (
), ?string $untrusted_certificates_file = NULL): int|bool {}
function openssl_x509_read(OpenSSLCertificate|string $certificate): OpenSSLCertificate|false {}
function openssl_x509_free(OpenSSLCertificate $certificate): void {}
function openssl_pkcs12_export_to_file(OpenSSLCertificate|string $certificate, string $output_filename, $private_key, string $passphrase, array $options = array (
)): bool {}
function openssl_pkcs12_export(OpenSSLCertificate|string $certificate, $output, $private_key, string $passphrase, array $options = array (
)): bool {}
function openssl_pkcs12_read(string $pkcs12, $certificates, string $passphrase): bool {}
function openssl_csr_export_to_file(OpenSSLCertificateSigningRequest|string $csr, string $output_filename, bool $no_text = true): bool {}
function openssl_csr_export(OpenSSLCertificateSigningRequest|string $csr, $output, bool $no_text = true): bool {}
function openssl_csr_sign(OpenSSLCertificateSigningRequest|string $csr, OpenSSLCertificate|string|null $ca_certificate, $private_key, int $days, ?array $options = NULL, int $serial = 0): OpenSSLCertificate|false {}
function openssl_csr_new(array $distinguished_names, $private_key, ?array $options = NULL, ?array $extra_attributes = NULL): OpenSSLCertificateSigningRequest|false {}
function openssl_csr_get_subject(OpenSSLCertificateSigningRequest|string $csr, bool $short_names = true): array|false {}
function openssl_csr_get_public_key(OpenSSLCertificateSigningRequest|string $csr, bool $short_names = true): OpenSSLAsymmetricKey|false {}
function openssl_pkey_new(?array $options = NULL): OpenSSLAsymmetricKey|false {}
function openssl_pkey_export_to_file($key, string $output_filename, ?string $passphrase = NULL, ?array $options = NULL): bool {}
function openssl_pkey_export($key, $output, ?string $passphrase = NULL, ?array $options = NULL): bool {}
function openssl_pkey_get_public($public_key): OpenSSLAsymmetricKey|false {}
function openssl_get_publickey($public_key): OpenSSLAsymmetricKey|false {}
function openssl_pkey_free(OpenSSLAsymmetricKey $key): void {}
function openssl_free_key(OpenSSLAsymmetricKey $key): void {}
function openssl_pkey_get_private($private_key, ?string $passphrase = NULL): OpenSSLAsymmetricKey|false {}
function openssl_get_privatekey($private_key, ?string $passphrase = NULL): OpenSSLAsymmetricKey|false {}
function openssl_pkey_get_details(OpenSSLAsymmetricKey $key): array|false {}
function openssl_pbkdf2(string $password, string $salt, int $key_length, int $iterations, string $digest_algo = 'sha1'): string|false {}
function openssl_pkcs7_verify(string $input_filename, int $flags, ?string $signers_certificates_filename = NULL, array $ca_info = array (
), ?string $untrusted_certificates_filename = NULL, ?string $content = NULL, ?string $output_filename = NULL): int|bool {}
function openssl_pkcs7_encrypt(string $input_filename, string $output_filename, $certificate, ?array $headers, int $flags = 0, int $cipher_algo = 5): bool {}
function openssl_pkcs7_sign(string $input_filename, string $output_filename, OpenSSLCertificate|string $certificate, $private_key, ?array $headers, int $flags = 64, ?string $untrusted_certificates_filename = NULL): bool {}
function openssl_pkcs7_decrypt(string $input_filename, string $output_filename, $certificate, $private_key = NULL): bool {}
function openssl_pkcs7_read(string $data, $certificates): bool {}
function openssl_cms_verify(string $input_filename, int $flags = 0, ?string $certificates = NULL, array $ca_info = array (
), ?string $untrusted_certificates_filename = NULL, ?string $content = NULL, ?string $pk7 = NULL, ?string $sigfile = NULL, int $encoding = 1): bool {}
function openssl_cms_encrypt(string $input_filename, string $output_filename, $certificate, ?array $headers, int $flags = 0, int $encoding = 1, int $cipher_algo = 5): bool {}
function openssl_cms_sign(string $input_filename, string $output_filename, OpenSSLCertificate|string $certificate, $private_key, ?array $headers, int $flags = 0, int $encoding = 1, ?string $untrusted_certificates_filename = NULL): bool {}
function openssl_cms_decrypt(string $input_filename, string $output_filename, $certificate, $private_key = NULL, int $encoding = 1): bool {}
function openssl_cms_read(string $input_filename, $certificates): bool {}
function openssl_private_encrypt(string $data, $encrypted_data, $private_key, int $padding = 1): bool {}
function openssl_private_decrypt(string $data, $decrypted_data, $private_key, int $padding = 1): bool {}
function openssl_public_encrypt(string $data, $encrypted_data, $public_key, int $padding = 1): bool {}
function openssl_public_decrypt(string $data, $decrypted_data, $public_key, int $padding = 1): bool {}
function openssl_error_string(): string|false {}
function openssl_sign(string $data, $signature, $private_key, string|int $algorithm = 1): bool {}
function openssl_verify(string $data, string $signature, $public_key, string|int $algorithm = 1): int|false {}
function openssl_seal(string $data, $sealed_data, $encrypted_keys, array $public_key, string $cipher_algo, $iv = NULL): int|false {}
function openssl_open(string $data, $output, string $encrypted_key, $private_key, string $cipher_algo, ?string $iv = NULL): bool {}
function openssl_get_md_methods(bool $aliases = false): array {}
function openssl_get_cipher_methods(bool $aliases = false): array {}
function openssl_get_curve_names(): array|false {}
function openssl_digest(string $data, string $digest_algo, bool $binary = false): string|false {}
function openssl_encrypt(string $data, string $cipher_algo, string $passphrase, int $options = 0, string $iv = '', $tag = NULL, string $aad = '', int $tag_length = 16): string|false {}
function openssl_decrypt(string $data, string $cipher_algo, string $passphrase, int $options = 0, string $iv = '', ?string $tag = NULL, string $aad = ''): string|false {}
function openssl_cipher_iv_length(string $cipher_algo): int|false {}
function openssl_cipher_key_length(string $cipher_algo): int|false {}
function openssl_dh_compute_key(string $public_key, OpenSSLAsymmetricKey $private_key): string|false {}
function openssl_pkey_derive($public_key, $private_key, int $key_length = 0): string|false {}
function openssl_random_pseudo_bytes(int $length, $strong_result = NULL): string {}
function openssl_spki_new(OpenSSLAsymmetricKey $private_key, string $challenge, int $digest_algo = 2): string|false {}
function openssl_spki_verify(string $spki): bool {}
function openssl_spki_export(string $spki): string|false {}
function openssl_spki_export_challenge(string $spki): string|false {}
function openssl_get_cert_locations(): array {}
function preg_match(string $pattern, string $subject, $matches = NULL, int $flags = 0, int $offset = 0): int|false {}
function preg_match_all(string $pattern, string $subject, $matches = NULL, int $flags = 0, int $offset = 0): int|false {}
function preg_replace(array|string $pattern, array|string $replacement, array|string $subject, int $limit = -1, $count = NULL): array|string|null {}
function preg_filter(array|string $pattern, array|string $replacement, array|string $subject, int $limit = -1, $count = NULL): array|string|null {}
function preg_replace_callback(array|string $pattern, callable $callback, array|string $subject, int $limit = -1, $count = NULL, int $flags = 0): array|string|null {}
function preg_replace_callback_array(array $pattern, array|string $subject, int $limit = -1, $count = NULL, int $flags = 0): array|string|null {}
function preg_split(string $pattern, string $subject, int $limit = -1, int $flags = 0): array|false {}
function preg_quote(string $str, ?string $delimiter = NULL): string {}
function preg_grep(string $pattern, array $array, int $flags = 0): array|false {}
function preg_last_error(): int {}
function preg_last_error_msg(): string {}
function ob_gzhandler(string $data, int $flags): string|false {}
function zlib_get_coding_type(): string|false {}
function gzfile(string $filename, int $use_include_path = 0): array|false {}
function gzopen(string $filename, string $mode, int $use_include_path = 0) {}
function readgzfile(string $filename, int $use_include_path = 0): int|false {}
function zlib_encode(string $data, int $encoding, int $level = -1): string|false {}
function zlib_decode(string $data, int $max_length = 0): string|false {}
function gzdeflate(string $data, int $level = -1, int $encoding = -15): string|false {}
function gzencode(string $data, int $level = -1, int $encoding = 31): string|false {}
function gzcompress(string $data, int $level = -1, int $encoding = 15): string|false {}
function gzinflate(string $data, int $max_length = 0): string|false {}
function gzdecode(string $data, int $max_length = 0): string|false {}
function gzuncompress(string $data, int $max_length = 0): string|false {}
function gzwrite($stream, string $data, ?int $length = NULL): int|false {}
function gzputs($stream, string $data, ?int $length = NULL): int|false {}
function gzrewind($stream): bool {}
function gzclose($stream): bool {}
function gzeof($stream): bool {}
function gzgetc($stream): string|false {}
function gzpassthru($stream): int {}
function gzseek($stream, int $offset, int $whence = 0): int {}
function gztell($stream): int|false {}
function gzread($stream, int $length): string|false {}
function gzgets($stream, ?int $length = NULL): string|false {}
function deflate_init(int $encoding, array $options = array (
)): DeflateContext|false {}
function deflate_add(DeflateContext $context, string $data, int $flush_mode = 2): string|false {}
function inflate_init(int $encoding, array $options = array (
)): InflateContext|false {}
function inflate_add(InflateContext $context, string $data, int $flush_mode = 2): string|false {}
function inflate_get_status(InflateContext $context): int {}
function inflate_get_read_len(InflateContext $context): int {}
function filter_has_var(int $input_type, string $var_name): bool {}
function filter_input(int $type, string $var_name, int $filter = 516, array|int $options = 0): mixed {}
function filter_var(mixed $value, int $filter = 516, array|int $options = 0): mixed {}
function filter_input_array(int $type, array|int $options = 516, bool $add_empty = true): array|false|null {}
function filter_var_array(array $array, array|int $options = 516, bool $add_empty = true): array|false|null {}
function filter_list(): array {}
function filter_id(string $name): int|false {}
function hash(string $algo, string $data, bool $binary = false, array $options = array (
)): string {}
function hash_file(string $algo, string $filename, bool $binary = false, array $options = array (
)): string|false {}
function hash_hmac(string $algo, string $data, string $key, bool $binary = false): string {}
function hash_hmac_file(string $algo, string $filename, string $key, bool $binary = false): string|false {}
function hash_init(string $algo, int $flags = 0, string $key = '', array $options = array (
)): HashContext {}
function hash_update(HashContext $context, string $data): bool {}
function hash_update_stream(HashContext $context, $stream, int $length = -1): int {}
function hash_update_file(HashContext $context, string $filename, $stream_context = NULL): bool {}
function hash_final(HashContext $context, bool $binary = false): string {}
function hash_copy(HashContext $context): HashContext {}
function hash_algos(): array {}
function hash_hmac_algos(): array {}
function hash_pbkdf2(string $algo, string $password, string $salt, int $iterations, int $length = 0, bool $binary = false): string {}
function hash_equals(string $known_string, string $user_string): bool {}
function hash_hkdf(string $algo, string $key, int $length = 0, string $info = '', string $salt = ''): string {}
function mhash_get_block_size(int $algo): int|false {}
function mhash_get_hash_name(int $algo): string|false {}
function mhash_keygen_s2k(int $algo, string $password, string $salt, int $length): string|false {}
function mhash_count(): int {}
function mhash(int $algo, string $data, ?string $key = NULL): string|false {}
function json_encode(mixed $value, int $flags = 0, int $depth = 512): string|false {}
function json_decode(string $json, ?bool $associative = NULL, int $depth = 512, int $flags = 0): mixed {}
function json_last_error(): int {}
function json_last_error_msg(): string {}
function pcntl_fork(): int {}
function pcntl_waitpid(int $process_id, $status, int $flags = 0, $resource_usage = array (
)): int {}
function pcntl_wait($status, int $flags = 0, $resource_usage = array (
)): int {}
function pcntl_signal(int $signal, $handler, bool $restart_syscalls = true): bool {}
function pcntl_signal_get_handler(int $signal) {}
function pcntl_signal_dispatch(): bool {}
function pcntl_sigprocmask(int $mode, array $signals, $old_signals = NULL): bool {}
function pcntl_sigwaitinfo(array $signals, $info = array (
)): int|false {}
function pcntl_sigtimedwait(array $signals, $info = array (
), int $seconds = 0, int $nanoseconds = 0): int|false {}
function pcntl_wifexited(int $status): bool {}
function pcntl_wifstopped(int $status): bool {}
function pcntl_wifcontinued(int $status): bool {}
function pcntl_wifsignaled(int $status): bool {}
function pcntl_wexitstatus(int $status): int|false {}
function pcntl_wtermsig(int $status): int|false {}
function pcntl_wstopsig(int $status): int|false {}
function pcntl_exec(string $path, array $args = array (
), array $env_vars = array (
)): bool {}
function pcntl_alarm(int $seconds): int {}
function pcntl_get_last_error(): int {}
function pcntl_errno(): int {}
function pcntl_getpriority(?int $process_id = NULL, int $mode = 0): int|false {}
function pcntl_setpriority(int $priority, ?int $process_id = NULL, int $mode = 0): bool {}
function pcntl_strerror(int $error_code): string {}
function pcntl_async_signals(?bool $enable = NULL): bool {}
function pcntl_unshare(int $flags): bool {}
function lcg_value(): float {}
function mt_srand(int $seed, int $mode = 0): void {}
function srand(int $seed, int $mode = 0): void {}
function rand(int $min, int $max): int {}
function mt_rand(int $min, int $max): int {}
function mt_getrandmax(): int {}
function getrandmax(): int {}
function random_bytes(int $length): string {}
function random_int(int $min, int $max): int {}
function set_time_limit(int $seconds): bool {}
function header_register_callback(callable $callback): bool {}
function ob_start($callback = NULL, int $chunk_size = 0, int $flags = 112): bool {}
function ob_flush(): bool {}
function ob_clean(): bool {}
function ob_end_flush(): bool {}
function ob_end_clean(): bool {}
function ob_get_flush(): string|false {}
function ob_get_clean(): string|false {}
function ob_get_contents(): string|false {}
function ob_get_level(): int {}
function ob_get_length(): int|false {}
function ob_list_handlers(): array {}
function ob_get_status(bool $full_status = false): array {}
function ob_implicit_flush(bool $enable = true): void {}
function output_reset_rewrite_vars(): bool {}
function output_add_rewrite_var(string $name, string $value): bool {}
function stream_wrapper_register(string $protocol, string $class, int $flags = 0): bool {}
function stream_register_wrapper(string $protocol, string $class, int $flags = 0): bool {}
function stream_wrapper_unregister(string $protocol): bool {}
function stream_wrapper_restore(string $protocol): bool {}
function array_push(array $array, mixed $values): int {}
function krsort(array $array, int $flags = 0): true {}
function ksort(array $array, int $flags = 0): true {}
function count(Countable|array $value, int $mode = 0): int {}
function sizeof(Countable|array $value, int $mode = 0): int {}
function natsort(array $array): bool {}
function natcasesort(array $array): bool {}
function asort(array $array, int $flags = 0): true {}
function arsort(array $array, int $flags = 0): true {}
function sort(array $array, int $flags = 0): true {}
function rsort(array $array, int $flags = 0): bool {}
function usort(array $array, callable $callback): true {}
function uasort(array $array, callable $callback): true {}
function uksort(array $array, callable $callback): true {}
function end(object|array $array): mixed {}
function prev(object|array $array): mixed {}
function next(object|array $array): mixed {}
function reset(object|array $array): mixed {}
function current(object|array $array): mixed {}
function pos(object|array $array): mixed {}
function key(object|array $array): string|int|null {}
function min(mixed $value, mixed $values): mixed {}
function max(mixed $value, mixed $values): mixed {}
function array_walk(object|array $array, callable $callback, mixed $arg): true {}
function array_walk_recursive(object|array $array, callable $callback, mixed $arg): true {}
function in_array(mixed $needle, array $haystack, bool $strict = false): bool {}
function array_search(mixed $needle, array $haystack, bool $strict = false): string|int|false {}
function extract(array $array, int $flags = 0, string $prefix = ''): int {}
function compact($var_name, $var_names): array {}
function array_fill(int $start_index, int $count, mixed $value): array {}
function array_fill_keys(array $keys, mixed $value): array {}
function range($start, $end, int|float $step = 1): array {}
function shuffle(array $array): true {}
function array_pop(array $array): mixed {}
function array_shift(array $array): mixed {}
function array_unshift(array $array, mixed $values): int {}
function array_splice(array $array, int $offset, ?int $length = NULL, mixed $replacement = array (
)): array {}
function array_slice(array $array, int $offset, ?int $length = NULL, bool $preserve_keys = false): array {}
function array_merge(array $arrays): array {}
function array_merge_recursive(array $arrays): array {}
function array_replace(array $array, array $replacements): array {}
function array_replace_recursive(array $array, array $replacements): array {}
function array_keys(array $array, mixed $filter_value, bool $strict = false): array {}
function array_key_first(array $array): string|int|null {}
function array_key_last(array $array): string|int|null {}
function array_values(array $array): array {}
function array_count_values(array $array): array {}
function array_column(array $array, string|int|null $column_key, string|int|null $index_key = NULL): array {}
function array_reverse(array $array, bool $preserve_keys = false): array {}
function array_pad(array $array, int $length, mixed $value): array {}
function array_flip(array $array): array {}
function array_change_key_case(array $array, int $case = 0): array {}
function array_unique(array $array, int $flags = 2): array {}
function array_intersect_key(array $array, array $arrays): array {}
function array_intersect_ukey(array $array, $rest): array {}
function array_intersect(array $array, array $arrays): array {}
function array_uintersect(array $array, $rest): array {}
function array_intersect_assoc(array $array, array $arrays): array {}
function array_uintersect_assoc(array $array, $rest): array {}
function array_intersect_uassoc(array $array, $rest): array {}
function array_uintersect_uassoc(array $array, $rest): array {}
function array_diff_key(array $array, array $arrays): array {}
function array_diff_ukey(array $array, $rest): array {}
function array_diff(array $array, array $arrays): array {}
function array_udiff(array $array, $rest): array {}
function array_diff_assoc(array $array, array $arrays): array {}
function array_diff_uassoc(array $array, $rest): array {}
function array_udiff_assoc(array $array, $rest): array {}
function array_udiff_uassoc(array $array, $rest): array {}
function array_multisort($array, $rest): bool {}
function array_rand(array $array, int $num = 1): array|string|int {}
function array_sum(array $array): int|float {}
function array_product(array $array): int|float {}
function array_reduce(array $array, callable $callback, mixed $initial = NULL): mixed {}
function array_filter(array $array, ?callable $callback = NULL, int $mode = 0): array {}
function array_map(?callable $callback, array $array, array $arrays): array {}
function array_key_exists($key, array $array): bool {}
function key_exists($key, array $array): bool {}
function array_chunk(array $array, int $length, bool $preserve_keys = false): array {}
function array_combine(array $keys, array $values): array {}
function array_is_list(array $array): bool {}
function base64_encode(string $string): string {}
function base64_decode(string $string, bool $strict = false): string|false {}
function constant(string $name): mixed {}
function ip2long(string $ip): int|false {}
function long2ip(int $ip): string|false {}
function getenv(?string $name = NULL, bool $local_only = false): array|string|false {}
function putenv(string $assignment): bool {}
function getopt(string $short_options, array $long_options = array (
), $rest_index = NULL): array|false {}
function flush(): void {}
function sleep(int $seconds): int {}
function usleep(int $microseconds): void {}
function time_nanosleep(int $seconds, int $nanoseconds): array|bool {}
function time_sleep_until(float $timestamp): bool {}
function get_current_user(): string {}
function get_cfg_var(string $option): array|string|false {}
function error_log(string $message, int $message_type = 0, ?string $destination = NULL, ?string $additional_headers = NULL): bool {}
function error_get_last(): ?array {}
function error_clear_last(): void {}
function call_user_func(callable $callback, mixed $args): mixed {}
function call_user_func_array(callable $callback, array $args): mixed {}
function forward_static_call(callable $callback, mixed $args): mixed {}
function forward_static_call_array(callable $callback, array $args): mixed {}
function register_shutdown_function(callable $callback, mixed $args): void {}
function highlight_file(string $filename, bool $return = false): string|bool {}
function show_source(string $filename, bool $return = false): string|bool {}
function php_strip_whitespace(string $filename): string {}
function highlight_string(string $string, bool $return = false): string|bool {}
function ini_get(string $option): string|false {}
function ini_get_all(?string $extension = NULL, bool $details = true): array|false {}
function ini_set(string $option, string|int|float|bool|null $value): string|false {}
function ini_alter(string $option, string|int|float|bool|null $value): string|false {}
function ini_restore(string $option): void {}
function ini_parse_quantity(string $shorthand): int {}
function set_include_path(string $include_path): string|false {}
function get_include_path(): string|false {}
function print_r(mixed $value, bool $return = false): string|bool {}
function connection_aborted(): int {}
function connection_status(): int {}
function ignore_user_abort(?bool $enable = NULL): int {}
function getservbyname(string $service, string $protocol): int|false {}
function getservbyport(int $port, string $protocol): string|false {}
function getprotobyname(string $protocol): int|false {}
function getprotobynumber(int $protocol): string|false {}
function register_tick_function(callable $callback, mixed $args): bool {}
function unregister_tick_function(callable $callback): void {}
function is_uploaded_file(string $filename): bool {}
function move_uploaded_file(string $from, string $to): bool {}
function parse_ini_file(string $filename, bool $process_sections = false, int $scanner_mode = 0): array|false {}
function parse_ini_string(string $ini_string, bool $process_sections = false, int $scanner_mode = 0): array|false {}
function sys_getloadavg(): array|false {}
function get_browser(?string $user_agent = NULL, bool $return_array = false): object|array|false {}
function crc32(string $string): int {}
function crypt(string $string, string $salt): string {}
function strptime(string $timestamp, string $format): array|false {}
function gethostname(): string|false {}
function gethostbyaddr(string $ip): string|false {}
function gethostbyname(string $hostname): string {}
function gethostbynamel(string $hostname): array|false {}
function dns_check_record(string $hostname, string $type = 'MX'): bool {}
function checkdnsrr(string $hostname, string $type = 'MX'): bool {}
function dns_get_record(string $hostname, int $type = 268435456, $authoritative_name_servers = NULL, $additional_records = NULL, bool $raw = false): array|false {}
function dns_get_mx(string $hostname, $hosts, $weights = NULL): bool {}
function getmxrr(string $hostname, $hosts, $weights = NULL): bool {}
function net_get_interfaces(): array|false {}
function ftok(string $filename, string $project_id): int {}
function hrtime(bool $as_number = false): array|int|float|false {}
function md5(string $string, bool $binary = false): string {}
function md5_file(string $filename, bool $binary = false): string|false {}
function getmyuid(): int|false {}
function getmygid(): int|false {}
function getmypid(): int|false {}
function getmyinode(): int|false {}
function getlastmod(): int|false {}
function sha1(string $string, bool $binary = false): string {}
function sha1_file(string $filename, bool $binary = false): string|false {}
function openlog(string $prefix, int $flags, int $facility): true {}
function closelog(): true {}
function syslog(int $priority, string $message): true {}
function inet_ntop(string $ip): string|false {}
function inet_pton(string $ip): string|false {}
function metaphone(string $string, int $max_phonemes = 0): string {}
function header(string $header, bool $replace = true, int $response_code = 0): void {}
function header_remove(?string $name = NULL): void {}
function setrawcookie(string $name, string $value = '', array|int $expires_or_options = 0, string $path = '', string $domain = '', bool $secure = false, bool $httponly = false): bool {}
function setcookie(string $name, string $value = '', array|int $expires_or_options = 0, string $path = '', string $domain = '', bool $secure = false, bool $httponly = false): bool {}
function http_response_code(int $response_code = 0): int|bool {}
function headers_sent($filename = NULL, $line = NULL): bool {}
function headers_list(): array {}
function htmlspecialchars(string $string, int $flags = 11, ?string $encoding = NULL, bool $double_encode = true): string {}
function htmlspecialchars_decode(string $string, int $flags = 11): string {}
function html_entity_decode(string $string, int $flags = 11, ?string $encoding = NULL): string {}
function htmlentities(string $string, int $flags = 11, ?string $encoding = NULL, bool $double_encode = true): string {}
function get_html_translation_table(int $table = 0, int $flags = 11, string $encoding = 'UTF-8'): array {}
function assert(mixed $assertion, Throwable|string|null $description = NULL): bool {}
function assert_options(int $option, mixed $value): mixed {}
function bin2hex(string $string): string {}
function hex2bin(string $string): string|false {}
function strspn(string $string, string $characters, int $offset = 0, ?int $length = NULL): int {}
function strcspn(string $string, string $characters, int $offset = 0, ?int $length = NULL): int {}
function nl_langinfo(int $item): string|false {}
function strcoll(string $string1, string $string2): int {}
function trim(string $string, string $characters = ' 
	' . "\0" . ''): string {}
function rtrim(string $string, string $characters = ' 
	' . "\0" . ''): string {}
function chop(string $string, string $characters = ' 
	' . "\0" . ''): string {}
function ltrim(string $string, string $characters = ' 
	' . "\0" . ''): string {}
function wordwrap(string $string, int $width = 75, string $break = '
', bool $cut_long_words = false): string {}
function explode(string $separator, string $string, int $limit = 9223372036854775807): array {}
function implode(array|string $separator, ?array $array = NULL): string {}
function join(array|string $separator, ?array $array = NULL): string {}
function strtok(string $string, ?string $token = NULL): string|false {}
function strtoupper(string $string): string {}
function strtolower(string $string): string {}
function basename(string $path, string $suffix = ''): string {}
function dirname(string $path, int $levels = 1): string {}
function pathinfo(string $path, int $flags = 15): array|string {}
function stristr(string $haystack, string $needle, bool $before_needle = false): string|false {}
function strstr(string $haystack, string $needle, bool $before_needle = false): string|false {}
function strchr(string $haystack, string $needle, bool $before_needle = false): string|false {}
function strpos(string $haystack, string $needle, int $offset = 0): int|false {}
function stripos(string $haystack, string $needle, int $offset = 0): int|false {}
function strrpos(string $haystack, string $needle, int $offset = 0): int|false {}
function strripos(string $haystack, string $needle, int $offset = 0): int|false {}
function strrchr(string $haystack, string $needle): string|false {}
function str_contains(string $haystack, string $needle): bool {}
function str_starts_with(string $haystack, string $needle): bool {}
function str_ends_with(string $haystack, string $needle): bool {}
function chunk_split(string $string, int $length = 76, string $separator = '
'): string {}
function substr(string $string, int $offset, ?int $length = NULL): string {}
function substr_replace(array|string $string, array|string $replace, array|int $offset, array|int|null $length = NULL): array|string {}
function quotemeta(string $string): string {}
function ord(string $character): int {}
function chr(int $codepoint): string {}
function ucfirst(string $string): string {}
function lcfirst(string $string): string {}
function ucwords(string $string, string $separators = ' 	
'): string {}
function strtr(string $string, array|string $from, ?string $to = NULL): string {}
function strrev(string $string): string {}
function similar_text(string $string1, string $string2, $percent = NULL): int {}
function addcslashes(string $string, string $characters): string {}
function addslashes(string $string): string {}
function stripcslashes(string $string): string {}
function stripslashes(string $string): string {}
function str_replace(array|string $search, array|string $replace, array|string $subject, $count = NULL): array|string {}
function str_ireplace(array|string $search, array|string $replace, array|string $subject, $count = NULL): array|string {}
function hebrev(string $string, int $max_chars_per_line = 0): string {}
function nl2br(string $string, bool $use_xhtml = true): string {}
function strip_tags(string $string, array|string|null $allowed_tags = NULL): string {}
function setlocale(int $category, $locales, $rest): string|false {}
function parse_str(string $string, $result): void {}
function str_getcsv(string $string, string $separator = ',', string $enclosure = '"', string $escape = '\\'): array {}
function str_repeat(string $string, int $times): string {}
function count_chars(string $string, int $mode = 0): array|string {}
function strnatcmp(string $string1, string $string2): int {}
function localeconv(): array {}
function strnatcasecmp(string $string1, string $string2): int {}
function substr_count(string $haystack, string $needle, int $offset = 0, ?int $length = NULL): int {}
function str_pad(string $string, int $length, string $pad_string = ' ', int $pad_type = 1): string {}
function sscanf(string $string, string $format, mixed $vars): array|int|null {}
function str_rot13(string $string): string {}
function str_shuffle(string $string): string {}
function str_word_count(string $string, int $format = 0, ?string $characters = NULL): array|int {}
function str_split(string $string, int $length = 1): array {}
function strpbrk(string $string, string $characters): string|false {}
function substr_compare(string $haystack, string $needle, int $offset, ?int $length = NULL, bool $case_insensitive = false): int {}
function utf8_encode(string $string): string {}
function utf8_decode(string $string): string {}
function opendir(string $directory, $context = NULL) {}
function dir(string $directory, $context = NULL): Directory|false {}
function closedir($dir_handle = NULL): void {}
function chdir(string $directory): bool {}
function chroot(string $directory): bool {}
function getcwd(): string|false {}
function rewinddir($dir_handle = NULL): void {}
function readdir($dir_handle = NULL): string|false {}
function scandir(string $directory, int $sorting_order = 0, $context = NULL): array|false {}
function glob(string $pattern, int $flags = 0): array|false {}
function exec(string $command, $output = NULL, $result_code = NULL): string|false {}
function system(string $command, $result_code = NULL): string|false {}
function passthru(string $command, $result_code = NULL): ?false {}
function escapeshellcmd(string $command): string {}
function escapeshellarg(string $arg): string {}
function shell_exec(string $command): string|false|null {}
function proc_nice(int $priority): bool {}
function flock($stream, int $operation, $would_block = NULL): bool {}
function get_meta_tags(string $filename, bool $use_include_path = false): array|false {}
function pclose($handle): int {}
function popen(string $command, string $mode) {}
function readfile(string $filename, bool $use_include_path = false, $context = NULL): int|false {}
function rewind($stream): bool {}
function rmdir(string $directory, $context = NULL): bool {}
function umask(?int $mask = NULL): int {}
function fclose($stream): bool {}
function feof($stream): bool {}
function fgetc($stream): string|false {}
function fgets($stream, ?int $length = NULL): string|false {}
function fread($stream, int $length): string|false {}
function fopen(string $filename, string $mode, bool $use_include_path = false, $context = NULL) {}
function fscanf($stream, string $format, mixed $vars): array|int|false|null {}
function fpassthru($stream): int {}
function ftruncate($stream, int $size): bool {}
function fstat($stream): array|false {}
function fseek($stream, int $offset, int $whence = 0): int {}
function ftell($stream): int|false {}
function fflush($stream): bool {}
function fsync($stream): bool {}
function fdatasync($stream): bool {}
function fwrite($stream, string $data, ?int $length = NULL): int|false {}
function fputs($stream, string $data, ?int $length = NULL): int|false {}
function mkdir(string $directory, int $permissions = 511, bool $recursive = false, $context = NULL): bool {}
function rename(string $from, string $to, $context = NULL): bool {}
function copy(string $from, string $to, $context = NULL): bool {}
function tempnam(string $directory, string $prefix): string|false {}
function tmpfile() {}
function file(string $filename, int $flags = 0, $context = NULL): array|false {}
function file_get_contents(string $filename, bool $use_include_path = false, $context = NULL, int $offset = 0, ?int $length = NULL): string|false {}
function unlink(string $filename, $context = NULL): bool {}
function file_put_contents(string $filename, mixed $data, int $flags = 0, $context = NULL): int|false {}
function fputcsv($stream, array $fields, string $separator = ',', string $enclosure = '"', string $escape = '\\', string $eol = '
'): int|false {}
function fgetcsv($stream, ?int $length = NULL, string $separator = ',', string $enclosure = '"', string $escape = '\\'): array|false {}
function realpath(string $path): string|false {}
function fnmatch(string $pattern, string $filename, int $flags = 0): bool {}
function sys_get_temp_dir(): string {}
function fileatime(string $filename): int|false {}
function filectime(string $filename): int|false {}
function filegroup(string $filename): int|false {}
function fileinode(string $filename): int|false {}
function filemtime(string $filename): int|false {}
function fileowner(string $filename): int|false {}
function fileperms(string $filename): int|false {}
function filesize(string $filename): int|false {}
function filetype(string $filename): string|false {}
function file_exists(string $filename): bool {}
function is_writable(string $filename): bool {}
function is_writeable(string $filename): bool {}
function is_readable(string $filename): bool {}
function is_executable(string $filename): bool {}
function is_file(string $filename): bool {}
function is_dir(string $filename): bool {}
function is_link(string $filename): bool {}
function stat(string $filename): array|false {}
function lstat(string $filename): array|false {}
function chown(string $filename, string|int $user): bool {}
function chgrp(string $filename, string|int $group): bool {}
function lchown(string $filename, string|int $user): bool {}
function lchgrp(string $filename, string|int $group): bool {}
function chmod(string $filename, int $permissions): bool {}
function touch(string $filename, ?int $mtime = NULL, ?int $atime = NULL): bool {}
function clearstatcache(bool $clear_realpath_cache = false, string $filename = ''): void {}
function disk_total_space(string $directory): float|false {}
function disk_free_space(string $directory): float|false {}
function diskfreespace(string $directory): float|false {}
function realpath_cache_get(): array {}
function realpath_cache_size(): int {}
function sprintf(string $format, mixed $values): string {}
function printf(string $format, mixed $values): int {}
function vprintf(string $format, array $values): int {}
function vsprintf(string $format, array $values): string {}
function fprintf($stream, string $format, mixed $values): int {}
function vfprintf($stream, string $format, array $values): int {}
function fsockopen(string $hostname, int $port = -1, $error_code = NULL, $error_message = NULL, ?float $timeout = NULL) {}
function pfsockopen(string $hostname, int $port = -1, $error_code = NULL, $error_message = NULL, ?float $timeout = NULL) {}
function http_build_query(object|array $data, string $numeric_prefix = '', ?string $arg_separator = NULL, int $encoding_type = 1): string {}
function image_type_to_mime_type(int $image_type): string {}
function image_type_to_extension(int $image_type, bool $include_dot = true): string|false {}
function getimagesize(string $filename, $image_info = NULL): array|false {}
function getimagesizefromstring(string $string, $image_info = NULL): array|false {}
function phpinfo(int $flags = 4294967295): true {}
function phpversion(?string $extension = NULL): string|false {}
function phpcredits(int $flags = 4294967295): true {}
function php_sapi_name(): string|false {}
function php_uname(string $mode = 'a'): string {}
function php_ini_scanned_files(): string|false {}
function php_ini_loaded_file(): string|false {}
function iptcembed(string $iptc_data, string $filename, int $spool = 0): string|bool {}
function iptcparse(string $iptc_block): array|false {}
function levenshtein(string $string1, string $string2, int $insertion_cost = 1, int $replacement_cost = 1, int $deletion_cost = 1): int {}
function readlink(string $path): string|false {}
function linkinfo(string $path): int|false {}
function symlink(string $target, string $link): bool {}
function link(string $target, string $link): bool {}
function mail(string $to, string $subject, string $message, array|string $additional_headers = array (
), string $additional_params = ''): bool {}
function abs(int|float $num): int|float {}
function ceil(int|float $num): float {}
function floor(int|float $num): float {}
function round(int|float $num, int $precision = 0, int $mode = 1): float {}
function sin(float $num): float {}
function cos(float $num): float {}
function tan(float $num): float {}
function asin(float $num): float {}
function acos(float $num): float {}
function atan(float $num): float {}
function atanh(float $num): float {}
function atan2(float $y, float $x): float {}
function sinh(float $num): float {}
function cosh(float $num): float {}
function tanh(float $num): float {}
function asinh(float $num): float {}
function acosh(float $num): float {}
function expm1(float $num): float {}
function log1p(float $num): float {}
function pi(): float {}
function is_finite(float $num): bool {}
function is_nan(float $num): bool {}
function intdiv(int $num1, int $num2): int {}
function is_infinite(float $num): bool {}
function pow(mixed $num, mixed $exponent): object|int|float {}
function exp(float $num): float {}
function log(float $num, float $base = 2.718281828459045): float {}
function log10(float $num): float {}
function sqrt(float $num): float {}
function hypot(float $x, float $y): float {}
function deg2rad(float $num): float {}
function rad2deg(float $num): float {}
function bindec(string $binary_string): int|float {}
function hexdec(string $hex_string): int|float {}
function octdec(string $octal_string): int|float {}
function decbin(int $num): string {}
function decoct(int $num): string {}
function dechex(int $num): string {}
function base_convert(string $num, int $from_base, int $to_base): string {}
function number_format(float $num, int $decimals = 0, ?string $decimal_separator = '.', ?string $thousands_separator = ','): string {}
function fmod(float $num1, float $num2): float {}
function fdiv(float $num1, float $num2): float {}
function microtime(bool $as_float = false): string|float {}
function gettimeofday(bool $as_float = false): array|float {}
function getrusage(int $mode = 0): array|false {}
function pack(string $format, mixed $values): string {}
function unpack(string $format, string $string, int $offset = 0): array|false {}
function password_get_info(string $hash): array {}
function password_hash(string $password, string|int|null $algo, array $options = array (
)): string {}
function password_needs_rehash(string $hash, string|int|null $algo, array $options = array (
)): bool {}
function password_verify(string $password, string $hash): bool {}
function password_algos(): array {}
function proc_open(array|string $command, array $descriptor_spec, $pipes, ?string $cwd = NULL, ?array $env_vars = NULL, ?array $options = NULL) {}
function proc_close($process): int {}
function proc_terminate($process, int $signal = 15): bool {}
function proc_get_status($process): array {}
function quoted_printable_decode(string $string): string {}
function quoted_printable_encode(string $string): string {}
function soundex(string $string): string {}
function stream_select(?array $read, ?array $write, ?array $except, ?int $seconds, ?int $microseconds = NULL): int|false {}
function stream_context_create(?array $options = NULL, ?array $params = NULL) {}
function stream_context_set_params($context, array $params): bool {}
function stream_context_get_params($context): array {}
function stream_context_set_option($context, array|string $wrapper_or_options, ?string $option_name = NULL, mixed $value): bool {}
function stream_context_get_options($stream_or_context): array {}
function stream_context_get_default(?array $options = NULL) {}
function stream_context_set_default(array $options) {}
function stream_filter_prepend($stream, string $filter_name, int $mode = 0, mixed $params) {}
function stream_filter_append($stream, string $filter_name, int $mode = 0, mixed $params) {}
function stream_filter_remove($stream_filter): bool {}
function stream_socket_client(string $address, $error_code = NULL, $error_message = NULL, ?float $timeout = NULL, int $flags = 4, $context = NULL) {}
function stream_socket_server(string $address, $error_code = NULL, $error_message = NULL, int $flags = 12, $context = NULL) {}
function stream_socket_accept($socket, ?float $timeout = NULL, $peer_name = NULL) {}
function stream_socket_get_name($socket, bool $remote): string|false {}
function stream_socket_recvfrom($socket, int $length, int $flags = 0, $address = NULL): string|false {}
function stream_socket_sendto($socket, string $data, int $flags = 0, string $address = ''): int|false {}
function stream_socket_enable_crypto($stream, bool $enable, ?int $crypto_method = NULL, $session_stream = NULL): int|bool {}
function stream_socket_shutdown($stream, int $mode): bool {}
function stream_socket_pair(int $domain, int $type, int $protocol): array|false {}
function stream_copy_to_stream($from, $to, ?int $length = NULL, int $offset = 0): int|false {}
function stream_get_contents($stream, ?int $length = NULL, int $offset = -1): string|false {}
function stream_supports_lock($stream): bool {}
function stream_set_write_buffer($stream, int $size): int {}
function set_file_buffer($stream, int $size): int {}
function stream_set_read_buffer($stream, int $size): int {}
function stream_set_blocking($stream, bool $enable): bool {}
function socket_set_blocking($stream, bool $enable): bool {}
function stream_get_meta_data($stream): array {}
function socket_get_status($stream): array {}
function stream_get_line($stream, int $length, string $ending = ''): string|false {}
function stream_resolve_include_path(string $filename): string|false {}
function stream_get_wrappers(): array {}
function stream_get_transports(): array {}
function stream_is_local($stream): bool {}
function stream_isatty($stream): bool {}
function stream_set_chunk_size($stream, int $size): int {}
function stream_set_timeout($stream, int $seconds, int $microseconds = 0): bool {}
function socket_set_timeout($stream, int $seconds, int $microseconds = 0): bool {}
function gettype(mixed $value): string {}
function get_debug_type(mixed $value): string {}
function settype(mixed $var, string $type): bool {}
function intval(mixed $value, int $base = 10): int {}
function floatval(mixed $value): float {}
function doubleval(mixed $value): float {}
function boolval(mixed $value): bool {}
function strval(mixed $value): string {}
function is_null(mixed $value): bool {}
function is_resource(mixed $value): bool {}
function is_bool(mixed $value): bool {}
function is_int(mixed $value): bool {}
function is_integer(mixed $value): bool {}
function is_long(mixed $value): bool {}
function is_float(mixed $value): bool {}
function is_double(mixed $value): bool {}
function is_numeric(mixed $value): bool {}
function is_string(mixed $value): bool {}
function is_array(mixed $value): bool {}
function is_object(mixed $value): bool {}
function is_scalar(mixed $value): bool {}
function is_callable(mixed $value, bool $syntax_only = false, $callable_name = NULL): bool {}
function is_iterable(mixed $value): bool {}
function is_countable(mixed $value): bool {}
function uniqid(string $prefix = '', bool $more_entropy = false): string {}
function parse_url(string $url, int $component = -1): array|string|int|false|null {}
function urlencode(string $string): string {}
function urldecode(string $string): string {}
function rawurlencode(string $string): string {}
function rawurldecode(string $string): string {}
function get_headers(string $url, bool $associative = false, $context = NULL): array|false {}
function stream_bucket_make_writeable($brigade): ?object {}
function stream_bucket_prepend($brigade, object $bucket): void {}
function stream_bucket_append($brigade, object $bucket): void {}
function stream_bucket_new($stream, string $buffer): object {}
function stream_get_filters(): array {}
function stream_filter_register(string $filter_name, string $class): bool {}
function convert_uuencode(string $string): string {}
function convert_uudecode(string $string): string|false {}
function var_dump(mixed $value, mixed $values): void {}
function var_export(mixed $value, bool $return = false): ?string {}
function debug_zval_dump(mixed $value, mixed $values): void {}
function serialize(mixed $value): string {}
function unserialize(string $data, array $options = array (
)): mixed {}
function memory_get_usage(bool $real_usage = false): int {}
function memory_get_peak_usage(bool $real_usage = false): int {}
function memory_reset_peak_usage(): void {}
function version_compare(string $version1, string $version2, ?string $operator = NULL): int|bool {}
function class_implements($object_or_class, bool $autoload = true): array|false {}
function class_parents($object_or_class, bool $autoload = true): array|false {}
function class_uses($object_or_class, bool $autoload = true): array|false {}
function spl_autoload(string $class, ?string $file_extensions = NULL): void {}
function spl_autoload_call(string $class): void {}
function spl_autoload_extensions(?string $file_extensions = NULL): string {}
function spl_autoload_functions(): array {}
function spl_autoload_register(?callable $callback = NULL, bool $throw = true, bool $prepend = false): bool {}
function spl_autoload_unregister(callable $callback): bool {}
function spl_classes(): array {}
function spl_object_hash(object $object): string {}
function spl_object_id(object $object): int {}
function iterator_apply(Traversable $iterator, callable $callback, ?array $args = NULL): int {}
function iterator_count(Traversable|array $iterator): int {}
function iterator_to_array(Traversable|array $iterator, bool $preserve_keys = true): array {}
function session_name(?string $name = NULL): string|false {}
function session_module_name(?string $module = NULL): string|false {}
function session_save_path(?string $path = NULL): string|false {}
function session_id(?string $id = NULL): string|false {}
function session_create_id(string $prefix = ''): string|false {}
function session_regenerate_id(bool $delete_old_session = false): bool {}
function session_decode(string $data): bool {}
function session_encode(): string|false {}
function session_destroy(): bool {}
function session_unset(): bool {}
function session_gc(): int|false {}
function session_get_cookie_params(): array {}
function session_write_close(): bool {}
function session_abort(): bool {}
function session_reset(): bool {}
function session_status(): int {}
function session_register_shutdown(): void {}
function session_commit(): bool {}
function session_set_save_handler($open, $close, callable $read, callable $write, callable $destroy, callable $gc, callable $create_sid, callable $validate_sid, callable $update_timestamp): bool {}
function session_cache_limiter(?string $value = NULL): string|false {}
function session_cache_expire(?int $value = NULL): int|false {}
function session_set_cookie_params(array|int $lifetime_or_options, ?string $path = NULL, ?string $domain = NULL, ?bool $secure = NULL, ?bool $httponly = NULL): bool {}
function session_start(array $options = array (
)): bool {}
function sodium_crypto_aead_aes256gcm_is_available(): bool {}
function sodium_crypto_aead_aes256gcm_decrypt(string $ciphertext, string $additional_data, string $nonce, string $key): string|false {}
function sodium_crypto_aead_aes256gcm_encrypt(string $message, string $additional_data, string $nonce, string $key): string {}
function sodium_crypto_aead_aes256gcm_keygen(): string {}
function sodium_crypto_aead_chacha20poly1305_decrypt(string $ciphertext, string $additional_data, string $nonce, string $key): string|false {}
function sodium_crypto_aead_chacha20poly1305_encrypt(string $message, string $additional_data, string $nonce, string $key): string {}
function sodium_crypto_aead_chacha20poly1305_keygen(): string {}
function sodium_crypto_aead_chacha20poly1305_ietf_decrypt(string $ciphertext, string $additional_data, string $nonce, string $key): string|false {}
function sodium_crypto_aead_chacha20poly1305_ietf_encrypt(string $message, string $additional_data, string $nonce, string $key): string {}
function sodium_crypto_aead_chacha20poly1305_ietf_keygen(): string {}
function sodium_crypto_aead_xchacha20poly1305_ietf_decrypt(string $ciphertext, string $additional_data, string $nonce, string $key): string|false {}
function sodium_crypto_aead_xchacha20poly1305_ietf_keygen(): string {}
function sodium_crypto_aead_xchacha20poly1305_ietf_encrypt(string $message, string $additional_data, string $nonce, string $key): string {}
function sodium_crypto_auth(string $message, string $key): string {}
function sodium_crypto_auth_keygen(): string {}
function sodium_crypto_auth_verify(string $mac, string $message, string $key): bool {}
function sodium_crypto_box(string $message, string $nonce, string $key_pair): string {}
function sodium_crypto_box_keypair(): string {}
function sodium_crypto_box_seed_keypair(string $seed): string {}
function sodium_crypto_box_keypair_from_secretkey_and_publickey(string $secret_key, string $public_key): string {}
function sodium_crypto_box_open(string $ciphertext, string $nonce, string $key_pair): string|false {}
function sodium_crypto_box_publickey(string $key_pair): string {}
function sodium_crypto_box_publickey_from_secretkey(string $secret_key): string {}
function sodium_crypto_box_seal(string $message, string $public_key): string {}
function sodium_crypto_box_seal_open(string $ciphertext, string $key_pair): string|false {}
function sodium_crypto_box_secretkey(string $key_pair): string {}
function sodium_crypto_core_ristretto255_add(string $p, string $q): string {}
function sodium_crypto_core_ristretto255_from_hash(string $s): string {}
function sodium_crypto_core_ristretto255_is_valid_point(string $s): bool {}
function sodium_crypto_core_ristretto255_random(): string {}
function sodium_crypto_core_ristretto255_scalar_add(string $x, string $y): string {}
function sodium_crypto_core_ristretto255_scalar_complement(string $s): string {}
function sodium_crypto_core_ristretto255_scalar_invert(string $s): string {}
function sodium_crypto_core_ristretto255_scalar_mul(string $x, string $y): string {}
function sodium_crypto_core_ristretto255_scalar_negate(string $s): string {}
function sodium_crypto_core_ristretto255_scalar_random(): string {}
function sodium_crypto_core_ristretto255_scalar_reduce(string $s): string {}
function sodium_crypto_core_ristretto255_scalar_sub(string $x, string $y): string {}
function sodium_crypto_core_ristretto255_sub(string $p, string $q): string {}
function sodium_crypto_kx_keypair(): string {}
function sodium_crypto_kx_publickey(string $key_pair): string {}
function sodium_crypto_kx_secretkey(string $key_pair): string {}
function sodium_crypto_kx_seed_keypair(string $seed): string {}
function sodium_crypto_kx_client_session_keys(string $client_key_pair, string $server_key): array {}
function sodium_crypto_kx_server_session_keys(string $server_key_pair, string $client_key): array {}
function sodium_crypto_generichash(string $message, string $key = '', int $length = 32): string {}
function sodium_crypto_generichash_keygen(): string {}
function sodium_crypto_generichash_init(string $key = '', int $length = 32): string {}
function sodium_crypto_generichash_update(string $state, string $message): true {}
function sodium_crypto_generichash_final(string $state, int $length = 32): string {}
function sodium_crypto_kdf_derive_from_key(int $subkey_length, int $subkey_id, string $context, string $key): string {}
function sodium_crypto_kdf_keygen(): string {}
function sodium_crypto_pwhash(int $length, string $password, string $salt, int $opslimit, int $memlimit, int $algo = 2): string {}
function sodium_crypto_pwhash_str(string $password, int $opslimit, int $memlimit): string {}
function sodium_crypto_pwhash_str_verify(string $hash, string $password): bool {}
function sodium_crypto_pwhash_str_needs_rehash(string $password, int $opslimit, int $memlimit): bool {}
function sodium_crypto_pwhash_scryptsalsa208sha256(int $length, string $password, string $salt, int $opslimit, int $memlimit): string {}
function sodium_crypto_pwhash_scryptsalsa208sha256_str(string $password, int $opslimit, int $memlimit): string {}
function sodium_crypto_pwhash_scryptsalsa208sha256_str_verify(string $hash, string $password): bool {}
function sodium_crypto_scalarmult(string $n, string $p): string {}
function sodium_crypto_scalarmult_ristretto255(string $n, string $p): string {}
function sodium_crypto_scalarmult_ristretto255_base(string $n): string {}
function sodium_crypto_secretbox(string $message, string $nonce, string $key): string {}
function sodium_crypto_secretbox_keygen(): string {}
function sodium_crypto_secretbox_open(string $ciphertext, string $nonce, string $key): string|false {}
function sodium_crypto_secretstream_xchacha20poly1305_keygen(): string {}
function sodium_crypto_secretstream_xchacha20poly1305_init_push(string $key): array {}
function sodium_crypto_secretstream_xchacha20poly1305_push(string $state, string $message, string $additional_data = '', int $tag = 0): string {}
function sodium_crypto_secretstream_xchacha20poly1305_init_pull(string $header, string $key): string {}
function sodium_crypto_secretstream_xchacha20poly1305_pull(string $state, string $ciphertext, string $additional_data = ''): array|false {}
function sodium_crypto_secretstream_xchacha20poly1305_rekey(string $state): void {}
function sodium_crypto_shorthash(string $message, string $key): string {}
function sodium_crypto_shorthash_keygen(): string {}
function sodium_crypto_sign(string $message, string $secret_key): string {}
function sodium_crypto_sign_detached(string $message, string $secret_key): string {}
function sodium_crypto_sign_ed25519_pk_to_curve25519(string $public_key): string {}
function sodium_crypto_sign_ed25519_sk_to_curve25519(string $secret_key): string {}
function sodium_crypto_sign_keypair(): string {}
function sodium_crypto_sign_keypair_from_secretkey_and_publickey(string $secret_key, string $public_key): string {}
function sodium_crypto_sign_open(string $signed_message, string $public_key): string|false {}
function sodium_crypto_sign_publickey(string $key_pair): string {}
function sodium_crypto_sign_secretkey(string $key_pair): string {}
function sodium_crypto_sign_publickey_from_secretkey(string $secret_key): string {}
function sodium_crypto_sign_seed_keypair(string $seed): string {}
function sodium_crypto_sign_verify_detached(string $signature, string $message, string $public_key): bool {}
function sodium_crypto_stream(int $length, string $nonce, string $key): string {}
function sodium_crypto_stream_keygen(): string {}
function sodium_crypto_stream_xor(string $message, string $nonce, string $key): string {}
function sodium_crypto_stream_xchacha20(int $length, string $nonce, string $key): string {}
function sodium_crypto_stream_xchacha20_keygen(): string {}
function sodium_crypto_stream_xchacha20_xor(string $message, string $nonce, string $key): string {}
function sodium_crypto_stream_xchacha20_xor_ic(string $message, string $nonce, int $counter, string $key): string {}
function sodium_add(string $string1, string $string2): void {}
function sodium_compare(string $string1, string $string2): int {}
function sodium_increment(string $string): void {}
function sodium_memcmp(string $string1, string $string2): int {}
function sodium_memzero(string $string): void {}
function sodium_pad(string $string, int $block_size): string {}
function sodium_unpad(string $string, int $block_size): string {}
function sodium_bin2hex(string $string): string {}
function sodium_hex2bin(string $string, string $ignore = ''): string {}
function sodium_bin2base64(string $string, int $id): string {}
function sodium_base642bin(string $string, int $id, string $ignore = ''): string {}
function sodium_crypto_scalarmult_base(string $secret_key): string {}
function pdo_drivers(): array {}
function xml_parser_create(?string $encoding = NULL): XMLParser {}
function xml_parser_create_ns(?string $encoding = NULL, string $separator = ':'): XMLParser {}
function xml_set_object(XMLParser $parser, object $object): true {}
function xml_set_element_handler(XMLParser $parser, $start_handler, $end_handler): true {}
function xml_set_character_data_handler(XMLParser $parser, $handler): true {}
function xml_set_processing_instruction_handler(XMLParser $parser, $handler): true {}
function xml_set_default_handler(XMLParser $parser, $handler): true {}
function xml_set_unparsed_entity_decl_handler(XMLParser $parser, $handler): true {}
function xml_set_notation_decl_handler(XMLParser $parser, $handler): true {}
function xml_set_external_entity_ref_handler(XMLParser $parser, $handler): true {}
function xml_set_start_namespace_decl_handler(XMLParser $parser, $handler): true {}
function xml_set_end_namespace_decl_handler(XMLParser $parser, $handler): true {}
function xml_parse(XMLParser $parser, string $data, bool $is_final = false): int {}
function xml_parse_into_struct(XMLParser $parser, string $data, $values, $index = NULL): int {}
function xml_get_error_code(XMLParser $parser): int {}
function xml_error_string(int $error_code): ?string {}
function xml_get_current_line_number(XMLParser $parser): int {}
function xml_get_current_column_number(XMLParser $parser): int {}
function xml_get_current_byte_index(XMLParser $parser): int {}
function xml_parser_free(XMLParser $parser): bool {}
function xml_parser_set_option(XMLParser $parser, int $option, $value): bool {}
function xml_parser_get_option(XMLParser $parser, int $option): string|int {}
function apcu_clear_cache(): bool {}
function apcu_cache_info(bool $limited = false): array|false {}
function apcu_key_info(string $key): ?array {}
function apcu_sma_info(bool $limited = false): array|false {}
function apcu_enabled(): bool {}
function apcu_store($key, mixed $value, int $ttl = 0): array|bool {}
function apcu_add($key, mixed $value, int $ttl = 0): array|bool {}
function apcu_inc(string $key, int $step = 1, $success = NULL, int $ttl = 0): int|false {}
function apcu_dec(string $key, int $step = 1, $success = NULL, int $ttl = 0): int|false {}
function apcu_cas(string $key, int $old, int $new): bool {}
function apcu_fetch($key, $success = NULL): mixed {}
function apcu_exists($key): array|bool {}
function apcu_delete($key): array|bool {}
function apcu_entry(string $key, callable $callback, int $ttl = 0): mixed {}
function bcadd(string $num1, string $num2, ?int $scale = NULL): string {}
function bcsub(string $num1, string $num2, ?int $scale = NULL): string {}
function bcmul(string $num1, string $num2, ?int $scale = NULL): string {}
function bcdiv(string $num1, string $num2, ?int $scale = NULL): string {}
function bcmod(string $num1, string $num2, ?int $scale = NULL): string {}
function bcpowmod(string $num, string $exponent, string $modulus, ?int $scale = NULL): string {}
function bcpow(string $num, string $exponent, ?int $scale = NULL): string {}
function bcsqrt(string $num, ?int $scale = NULL): string {}
function bccomp(string $num1, string $num2, ?int $scale = NULL): int {}
function bcscale(?int $scale = NULL): int {}
function bzopen($file, string $mode) {}
function bzread($bz, int $length = 1024): string|false {}
function bzwrite($bz, string $data, ?int $length = NULL): int|false {}
function bzflush($bz): bool {}
function bzclose($bz): bool {}
function bzerrno($bz): int {}
function bzerrstr($bz): string {}
function bzerror($bz): array {}
function bzcompress(string $data, int $block_size = 4, int $work_factor = 0): string|int {}
function bzdecompress(string $data, bool $use_less_memory = false): string|int|false {}
function cal_days_in_month(int $calendar, int $month, int $year): int {}
function cal_from_jd(int $julian_day, int $calendar): array {}
function cal_info(int $calendar = -1): array {}
function cal_to_jd(int $calendar, int $month, int $day, int $year): int {}
function easter_date(?int $year = NULL, int $mode = 0): int {}
function easter_days(?int $year = NULL, int $mode = 0): int {}
function frenchtojd(int $month, int $day, int $year): int {}
function gregoriantojd(int $month, int $day, int $year): int {}
function jddayofweek(int $julian_day, int $mode = 0): string|int {}
function jdmonthname(int $julian_day, int $mode): string {}
function jdtofrench(int $julian_day): string {}
function jdtogregorian(int $julian_day): string {}
function jdtojewish(int $julian_day, bool $hebrew = false, int $flags = 0): string {}
function jdtojulian(int $julian_day): string {}
function jdtounix(int $julian_day): int {}
function jewishtojd(int $month, int $day, int $year): int {}
function juliantojd(int $month, int $day, int $year): int {}
function unixtojd(?int $timestamp = NULL): int|false {}
function ctype_alnum(mixed $text): bool {}
function ctype_alpha(mixed $text): bool {}
function ctype_cntrl(mixed $text): bool {}
function ctype_digit(mixed $text): bool {}
function ctype_lower(mixed $text): bool {}
function ctype_graph(mixed $text): bool {}
function ctype_print(mixed $text): bool {}
function ctype_punct(mixed $text): bool {}
function ctype_space(mixed $text): bool {}
function ctype_upper(mixed $text): bool {}
function ctype_xdigit(mixed $text): bool {}
function curl_close(CurlHandle $handle): void {}
function curl_copy_handle(CurlHandle $handle): CurlHandle|false {}
function curl_errno(CurlHandle $handle): int {}
function curl_error(CurlHandle $handle): string {}
function curl_escape(CurlHandle $handle, string $string): string|false {}
function curl_unescape(CurlHandle $handle, string $string): string|false {}
function curl_multi_setopt(CurlMultiHandle $multi_handle, int $option, mixed $value): bool {}
function curl_exec(CurlHandle $handle): string|bool {}
function curl_file_create(string $filename, ?string $mime_type = NULL, ?string $posted_filename = NULL): CURLFile {}
function curl_getinfo(CurlHandle $handle, ?int $option = NULL): mixed {}
function curl_init(?string $url = NULL): CurlHandle|false {}
function curl_upkeep(CurlHandle $handle): bool {}
function curl_multi_add_handle(CurlMultiHandle $multi_handle, CurlHandle $handle): int {}
function curl_multi_close(CurlMultiHandle $multi_handle): void {}
function curl_multi_errno(CurlMultiHandle $multi_handle): int {}
function curl_multi_exec(CurlMultiHandle $multi_handle, $still_running): int {}
function curl_multi_getcontent(CurlHandle $handle): ?string {}
function curl_multi_info_read(CurlMultiHandle $multi_handle, $queued_messages = NULL): array|false {}
function curl_multi_init(): CurlMultiHandle {}
function curl_multi_remove_handle(CurlMultiHandle $multi_handle, CurlHandle $handle): int {}
function curl_multi_select(CurlMultiHandle $multi_handle, float $timeout = 1.0): int {}
function curl_multi_strerror(int $error_code): ?string {}
function curl_pause(CurlHandle $handle, int $flags): int {}
function curl_reset(CurlHandle $handle): void {}
function curl_setopt_array(CurlHandle $handle, array $options): bool {}
function curl_setopt(CurlHandle $handle, int $option, mixed $value): bool {}
function curl_share_close(CurlShareHandle $share_handle): void {}
function curl_share_errno(CurlShareHandle $share_handle): int {}
function curl_share_init(): CurlShareHandle {}
function curl_share_setopt(CurlShareHandle $share_handle, int $option, mixed $value): bool {}
function curl_share_strerror(int $error_code): ?string {}
function curl_strerror(int $error_code): ?string {}
function curl_version(): array|false {}
function dom_import_simplexml(object $node): DOMElement {}
function exif_tagname(int $index): string|false {}
function exif_read_data($file, ?string $required_sections = NULL, bool $as_arrays = false, bool $read_thumbnail = false): array|false {}
function exif_thumbnail($file, $width = NULL, $height = NULL, $image_type = NULL): string|false {}
function exif_imagetype(string $filename): int|false {}
function finfo_open(int $flags = 0, ?string $magic_database = NULL): finfo|false {}
function finfo_close(finfo $finfo): bool {}
function finfo_set_flags(finfo $finfo, int $flags): bool {}
function finfo_file(finfo $finfo, string $filename, int $flags = 0, $context = NULL): string|false {}
function finfo_buffer(finfo $finfo, string $string, int $flags = 0, $context = NULL): string|false {}
function mime_content_type($filename): string|false {}
function ftp_connect(string $hostname, int $port = 21, int $timeout = 90): FTP\Connection|false {}
function ftp_ssl_connect(string $hostname, int $port = 21, int $timeout = 90): FTP\Connection|false {}
function ftp_login(FTP\Connection $ftp, string $username, string $password): bool {}
function ftp_pwd(FTP\Connection $ftp): string|false {}
function ftp_cdup(FTP\Connection $ftp): bool {}
function ftp_chdir(FTP\Connection $ftp, string $directory): bool {}
function ftp_exec(FTP\Connection $ftp, string $command): bool {}
function ftp_raw(FTP\Connection $ftp, string $command): ?array {}
function ftp_mkdir(FTP\Connection $ftp, string $directory): string|false {}
function ftp_rmdir(FTP\Connection $ftp, string $directory): bool {}
function ftp_chmod(FTP\Connection $ftp, int $permissions, string $filename): int|false {}
function ftp_alloc(FTP\Connection $ftp, int $size, $response = NULL): bool {}
function ftp_nlist(FTP\Connection $ftp, string $directory): array|false {}
function ftp_rawlist(FTP\Connection $ftp, string $directory, bool $recursive = false): array|false {}
function ftp_mlsd(FTP\Connection $ftp, string $directory): array|false {}
function ftp_systype(FTP\Connection $ftp): string|false {}
function ftp_fget(FTP\Connection $ftp, $stream, string $remote_filename, int $mode = 2, int $offset = 0): bool {}
function ftp_nb_fget(FTP\Connection $ftp, $stream, string $remote_filename, int $mode = 2, int $offset = 0): int {}
function ftp_pasv(FTP\Connection $ftp, bool $enable): bool {}
function ftp_get(FTP\Connection $ftp, string $local_filename, string $remote_filename, int $mode = 2, int $offset = 0): bool {}
function ftp_nb_get(FTP\Connection $ftp, string $local_filename, string $remote_filename, int $mode = 2, int $offset = 0): int|false {}
function ftp_nb_continue(FTP\Connection $ftp): int {}
function ftp_fput(FTP\Connection $ftp, string $remote_filename, $stream, int $mode = 2, int $offset = 0): bool {}
function ftp_nb_fput(FTP\Connection $ftp, string $remote_filename, $stream, int $mode = 2, int $offset = 0): int {}
function ftp_put(FTP\Connection $ftp, string $remote_filename, string $local_filename, int $mode = 2, int $offset = 0): bool {}
function ftp_append(FTP\Connection $ftp, string $remote_filename, string $local_filename, int $mode = 2): bool {}
function ftp_nb_put(FTP\Connection $ftp, string $remote_filename, string $local_filename, int $mode = 2, int $offset = 0): int|false {}
function ftp_size(FTP\Connection $ftp, string $filename): int {}
function ftp_mdtm(FTP\Connection $ftp, string $filename): int {}
function ftp_rename(FTP\Connection $ftp, string $from, string $to): bool {}
function ftp_delete(FTP\Connection $ftp, string $filename): bool {}
function ftp_site(FTP\Connection $ftp, string $command): bool {}
function ftp_close(FTP\Connection $ftp): bool {}
function ftp_quit(FTP\Connection $ftp): bool {}
function ftp_set_option(FTP\Connection $ftp, int $option, $value): bool {}
function ftp_get_option(FTP\Connection $ftp, int $option): int|bool {}
function gd_info(): array {}
function imageloadfont(string $filename): GdFont|false {}
function imagesetstyle(GdImage $image, array $style): bool {}
function imagecreatetruecolor(int $width, int $height): GdImage|false {}
function imageistruecolor(GdImage $image): bool {}
function imagetruecolortopalette(GdImage $image, bool $dither, int $num_colors): bool {}
function imagepalettetotruecolor(GdImage $image): bool {}
function imagecolormatch(GdImage $image1, GdImage $image2): bool {}
function imagesetthickness(GdImage $image, int $thickness): bool {}
function imagefilledellipse(GdImage $image, int $center_x, int $center_y, int $width, int $height, int $color): bool {}
function imagefilledarc(GdImage $image, int $center_x, int $center_y, int $width, int $height, int $start_angle, int $end_angle, int $color, int $style): bool {}
function imagealphablending(GdImage $image, bool $enable): bool {}
function imagesavealpha(GdImage $image, bool $enable): bool {}
function imagelayereffect(GdImage $image, int $effect): bool {}
function imagecolorallocatealpha(GdImage $image, int $red, int $green, int $blue, int $alpha): int|false {}
function imagecolorresolvealpha(GdImage $image, int $red, int $green, int $blue, int $alpha): int {}
function imagecolorclosestalpha(GdImage $image, int $red, int $green, int $blue, int $alpha): int {}
function imagecolorexactalpha(GdImage $image, int $red, int $green, int $blue, int $alpha): int {}
function imagecopyresampled(GdImage $dst_image, GdImage $src_image, int $dst_x, int $dst_y, int $src_x, int $src_y, int $dst_width, int $dst_height, int $src_width, int $src_height): bool {}
function imagerotate(GdImage $image, float $angle, int $background_color, bool $ignore_transparent = false): GdImage|false {}
function imagesettile(GdImage $image, GdImage $tile): bool {}
function imagesetbrush(GdImage $image, GdImage $brush): bool {}
function imagecreate(int $width, int $height): GdImage|false {}
function imagetypes(): int {}
function imagecreatefromstring(string $data): GdImage|false {}
function imagecreatefromavif(string $filename): GdImage|false {}
function imagecreatefromgif(string $filename): GdImage|false {}
function imagecreatefromjpeg(string $filename): GdImage|false {}
function imagecreatefrompng(string $filename): GdImage|false {}
function imagecreatefromwebp(string $filename): GdImage|false {}
function imagecreatefromxbm(string $filename): GdImage|false {}
function imagecreatefromxpm(string $filename): GdImage|false {}
function imagecreatefromwbmp(string $filename): GdImage|false {}
function imagecreatefromgd(string $filename): GdImage|false {}
function imagecreatefromgd2(string $filename): GdImage|false {}
function imagecreatefromgd2part(string $filename, int $x, int $y, int $width, int $height): GdImage|false {}
function imagecreatefrombmp(string $filename): GdImage|false {}
function imagecreatefromtga(string $filename): GdImage|false {}
function imagexbm(GdImage $image, ?string $filename, ?int $foreground_color = NULL): bool {}
function imageavif(GdImage $image, $file = NULL, int $quality = -1, int $speed = -1): bool {}
function imagegif(GdImage $image, $file = NULL): bool {}
function imagepng(GdImage $image, $file = NULL, int $quality = -1, int $filters = -1): bool {}
function imagewebp(GdImage $image, $file = NULL, int $quality = -1): bool {}
function imagejpeg(GdImage $image, $file = NULL, int $quality = -1): bool {}
function imagewbmp(GdImage $image, $file = NULL, ?int $foreground_color = NULL): bool {}
function imagegd(GdImage $image, ?string $file = NULL): bool {}
function imagegd2(GdImage $image, ?string $file = NULL, int $chunk_size, int $mode): bool {}
function imagebmp(GdImage $image, $file = NULL, bool $compressed = true): bool {}
function imagedestroy(GdImage $image): bool {}
function imagecolorallocate(GdImage $image, int $red, int $green, int $blue): int|false {}
function imagepalettecopy(GdImage $dst, GdImage $src): void {}
function imagecolorat(GdImage $image, int $x, int $y): int|false {}
function imagecolorclosest(GdImage $image, int $red, int $green, int $blue): int {}
function imagecolorclosesthwb(GdImage $image, int $red, int $green, int $blue): int {}
function imagecolordeallocate(GdImage $image, int $color): bool {}
function imagecolorresolve(GdImage $image, int $red, int $green, int $blue): int {}
function imagecolorexact(GdImage $image, int $red, int $green, int $blue): int {}
function imagecolorset(GdImage $image, int $color, int $red, int $green, int $blue, int $alpha = 0): ?false {}
function imagecolorsforindex(GdImage $image, int $color): array {}
function imagegammacorrect(GdImage $image, float $input_gamma, float $output_gamma): bool {}
function imagesetpixel(GdImage $image, int $x, int $y, int $color): bool {}
function imageline(GdImage $image, int $x1, int $y1, int $x2, int $y2, int $color): bool {}
function imagedashedline(GdImage $image, int $x1, int $y1, int $x2, int $y2, int $color): bool {}
function imagerectangle(GdImage $image, int $x1, int $y1, int $x2, int $y2, int $color): bool {}
function imagefilledrectangle(GdImage $image, int $x1, int $y1, int $x2, int $y2, int $color): bool {}
function imagearc(GdImage $image, int $center_x, int $center_y, int $width, int $height, int $start_angle, int $end_angle, int $color): bool {}
function imageellipse(GdImage $image, int $center_x, int $center_y, int $width, int $height, int $color): bool {}
function imagefilltoborder(GdImage $image, int $x, int $y, int $border_color, int $color): bool {}
function imagefill(GdImage $image, int $x, int $y, int $color): bool {}
function imagecolorstotal(GdImage $image): int {}
function imagecolortransparent(GdImage $image, ?int $color = NULL): int {}
function imageinterlace(GdImage $image, ?bool $enable = NULL): bool {}
function imagepolygon(GdImage $image, array $points, int $num_points_or_color, ?int $color = NULL): bool {}
function imageopenpolygon(GdImage $image, array $points, int $num_points_or_color, ?int $color = NULL): bool {}
function imagefilledpolygon(GdImage $image, array $points, int $num_points_or_color, ?int $color = NULL): bool {}
function imagefontwidth(GdFont|int $font): int {}
function imagefontheight(GdFont|int $font): int {}
function imagechar(GdImage $image, GdFont|int $font, int $x, int $y, string $char, int $color): bool {}
function imagecharup(GdImage $image, GdFont|int $font, int $x, int $y, string $char, int $color): bool {}
function imagestring(GdImage $image, GdFont|int $font, int $x, int $y, string $string, int $color): bool {}
function imagestringup(GdImage $image, GdFont|int $font, int $x, int $y, string $string, int $color): bool {}
function imagecopy(GdImage $dst_image, GdImage $src_image, int $dst_x, int $dst_y, int $src_x, int $src_y, int $src_width, int $src_height): bool {}
function imagecopymerge(GdImage $dst_image, GdImage $src_image, int $dst_x, int $dst_y, int $src_x, int $src_y, int $src_width, int $src_height, int $pct): bool {}
function imagecopymergegray(GdImage $dst_image, GdImage $src_image, int $dst_x, int $dst_y, int $src_x, int $src_y, int $src_width, int $src_height, int $pct): bool {}
function imagecopyresized(GdImage $dst_image, GdImage $src_image, int $dst_x, int $dst_y, int $src_x, int $src_y, int $dst_width, int $dst_height, int $src_width, int $src_height): bool {}
function imagesx(GdImage $image): int {}
function imagesy(GdImage $image): int {}
function imagesetclip(GdImage $image, int $x1, int $y1, int $x2, int $y2): bool {}
function imagegetclip(GdImage $image): array {}
function imageftbbox(float $size, float $angle, string $font_filename, string $string, array $options = array (
)): array|false {}
function imagefttext(GdImage $image, float $size, float $angle, int $x, int $y, int $color, string $font_filename, string $text, array $options = array (
)): array|false {}
function imagettfbbox(float $size, float $angle, string $font_filename, string $string, array $options = array (
)): array|false {}
function imagettftext(GdImage $image, float $size, float $angle, int $x, int $y, int $color, string $font_filename, string $text, array $options = array (
)): array|false {}
function imagefilter(GdImage $image, int $filter, $args): bool {}
function imageconvolution(GdImage $image, array $matrix, float $divisor, float $offset): bool {}
function imageflip(GdImage $image, int $mode): bool {}
function imageantialias(GdImage $image, bool $enable): bool {}
function imagecrop(GdImage $image, array $rectangle): GdImage|false {}
function imagecropauto(GdImage $image, int $mode = 0, float $threshold = 0.5, int $color = -1): GdImage|false {}
function imagescale(GdImage $image, int $width, int $height = -1, int $mode = 3): GdImage|false {}
function imageaffine(GdImage $image, array $affine, ?array $clip = NULL): GdImage|false {}
function imageaffinematrixget(int $type, $options): array|false {}
function imageaffinematrixconcat(array $matrix1, array $matrix2): array|false {}
function imagegetinterpolation(GdImage $image): int {}
function imagesetinterpolation(GdImage $image, int $method = 3): bool {}
function imageresolution(GdImage $image, ?int $resolution_x = NULL, ?int $resolution_y = NULL): array|bool {}
function textdomain(?string $domain): string {}
function gettext(string $message): string {}
function _(string $message): string {}
function dgettext(string $domain, string $message): string {}
function dcgettext(string $domain, string $message, int $category): string {}
function bindtextdomain(string $domain, ?string $directory): string|false {}
function ngettext(string $singular, string $plural, int $count): string {}
function dngettext(string $domain, string $singular, string $plural, int $count): string {}
function dcngettext(string $domain, string $singular, string $plural, int $count, int $category): string {}
function bind_textdomain_codeset(string $domain, ?string $codeset): string|false {}
function gmp_init(string|int $num, int $base = 0): GMP {}
function gmp_import(string $data, int $word_size = 1, int $flags = 17): GMP {}
function gmp_export(GMP|string|int $num, int $word_size = 1, int $flags = 17): string {}
function gmp_intval(GMP|string|int $num): int {}
function gmp_strval(GMP|string|int $num, int $base = 10): string {}
function gmp_add(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_sub(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_mul(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_div_qr(GMP|string|int $num1, GMP|string|int $num2, int $rounding_mode = 0): array {}
function gmp_div_q(GMP|string|int $num1, GMP|string|int $num2, int $rounding_mode = 0): GMP {}
function gmp_div_r(GMP|string|int $num1, GMP|string|int $num2, int $rounding_mode = 0): GMP {}
function gmp_div(GMP|string|int $num1, GMP|string|int $num2, int $rounding_mode = 0): GMP {}
function gmp_mod(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_divexact(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_neg(GMP|string|int $num): GMP {}
function gmp_abs(GMP|string|int $num): GMP {}
function gmp_fact(GMP|string|int $num): GMP {}
function gmp_sqrt(GMP|string|int $num): GMP {}
function gmp_sqrtrem(GMP|string|int $num): array {}
function gmp_root(GMP|string|int $num, int $nth): GMP {}
function gmp_rootrem(GMP|string|int $num, int $nth): array {}
function gmp_pow(GMP|string|int $num, int $exponent): GMP {}
function gmp_powm(GMP|string|int $num, GMP|string|int $exponent, GMP|string|int $modulus): GMP {}
function gmp_perfect_square(GMP|string|int $num): bool {}
function gmp_perfect_power(GMP|string|int $num): bool {}
function gmp_prob_prime(GMP|string|int $num, int $repetitions = 10): int {}
function gmp_gcd(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_gcdext(GMP|string|int $num1, GMP|string|int $num2): array {}
function gmp_lcm(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_invert(GMP|string|int $num1, GMP|string|int $num2): GMP|false {}
function gmp_jacobi(GMP|string|int $num1, GMP|string|int $num2): int {}
function gmp_legendre(GMP|string|int $num1, GMP|string|int $num2): int {}
function gmp_kronecker(GMP|string|int $num1, GMP|string|int $num2): int {}
function gmp_cmp(GMP|string|int $num1, GMP|string|int $num2): int {}
function gmp_sign(GMP|string|int $num): int {}
function gmp_random_seed(GMP|string|int $seed): void {}
function gmp_random_bits(int $bits): GMP {}
function gmp_random_range(GMP|string|int $min, GMP|string|int $max): GMP {}
function gmp_and(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_or(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_com(GMP|string|int $num): GMP {}
function gmp_xor(GMP|string|int $num1, GMP|string|int $num2): GMP {}
function gmp_setbit(GMP $num, int $index, bool $value = true): void {}
function gmp_clrbit(GMP $num, int $index): void {}
function gmp_testbit(GMP|string|int $num, int $index): bool {}
function gmp_scan0(GMP|string|int $num1, int $start): int {}
function gmp_scan1(GMP|string|int $num1, int $start): int {}
function gmp_popcount(GMP|string|int $num): int {}
function gmp_hamdist(GMP|string|int $num1, GMP|string|int $num2): int {}
function gmp_nextprime(GMP|string|int $num): GMP {}
function gmp_binomial(GMP|string|int $n, int $k): GMP {}
function iconv_strlen(string $string, ?string $encoding = NULL): int|false {}
function iconv_substr(string $string, int $offset, ?int $length = NULL, ?string $encoding = NULL): string|false {}
function iconv_strpos(string $haystack, string $needle, int $offset = 0, ?string $encoding = NULL): int|false {}
function iconv_strrpos(string $haystack, string $needle, ?string $encoding = NULL): int|false {}
function iconv_mime_encode(string $field_name, string $field_value, array $options = array (
)): string|false {}
function iconv_mime_decode(string $string, int $mode = 0, ?string $encoding = NULL): string|false {}
function iconv_mime_decode_headers(string $headers, int $mode = 0, ?string $encoding = NULL): array|false {}
function iconv(string $from_encoding, string $to_encoding, string $string): string|false {}
function iconv_set_encoding(string $type, string $encoding): bool {}
function iconv_get_encoding(string $type = 'all'): array|string|false {}
function igbinary_serialize($value) {}
function igbinary_unserialize($str) {}
function intlcal_create_instance($timezone = NULL, ?string $locale = NULL): ?IntlCalendar {}
function intlcal_get_keyword_values_for_locale(string $keyword, string $locale, bool $onlyCommon): IntlIterator|false {}
function intlcal_get_now(): float {}
function intlcal_get_available_locales(): array {}
function intlcal_get(IntlCalendar $calendar, int $field): int|false {}
function intlcal_get_time(IntlCalendar $calendar): float|false {}
function intlcal_set_time(IntlCalendar $calendar, float $timestamp): bool {}
function intlcal_add(IntlCalendar $calendar, int $field, int $value): bool {}
function intlcal_set_time_zone(IntlCalendar $calendar, $timezone): bool {}
function intlcal_after(IntlCalendar $calendar, IntlCalendar $other): bool {}
function intlcal_before(IntlCalendar $calendar, IntlCalendar $other): bool {}
function intlcal_set(IntlCalendar $calendar, int $year, int $month, int $dayOfMonth, int $hour, int $minute, int $second): bool {}
function intlcal_roll(IntlCalendar $calendar, int $field, $value): bool {}
function intlcal_clear(IntlCalendar $calendar, ?int $field = NULL): bool {}
function intlcal_field_difference(IntlCalendar $calendar, float $timestamp, int $field): int|false {}
function intlcal_get_actual_maximum(IntlCalendar $calendar, int $field): int|false {}
function intlcal_get_actual_minimum(IntlCalendar $calendar, int $field): int|false {}
function intlcal_get_day_of_week_type(IntlCalendar $calendar, int $dayOfWeek): int|false {}
function intlcal_get_first_day_of_week(IntlCalendar $calendar): int|false {}
function intlcal_get_least_maximum(IntlCalendar $calendar, int $field): int|false {}
function intlcal_get_greatest_minimum(IntlCalendar $calendar, int $field): int|false {}
function intlcal_get_locale(IntlCalendar $calendar, int $type): string|false {}
function intlcal_get_maximum(IntlCalendar $calendar, int $field): int|false {}
function intlcal_get_minimal_days_in_first_week(IntlCalendar $calendar): int|false {}
function intlcal_set_minimal_days_in_first_week(IntlCalendar $calendar, int $days): bool {}
function intlcal_get_minimum(IntlCalendar $calendar, int $field): int|false {}
function intlcal_get_time_zone(IntlCalendar $calendar): IntlTimeZone|false {}
function intlcal_get_type(IntlCalendar $calendar): string {}
function intlcal_get_weekend_transition(IntlCalendar $calendar, int $dayOfWeek): int|false {}
function intlcal_in_daylight_time(IntlCalendar $calendar): bool {}
function intlcal_is_lenient(IntlCalendar $calendar): bool {}
function intlcal_is_set(IntlCalendar $calendar, int $field): bool {}
function intlcal_is_equivalent_to(IntlCalendar $calendar, IntlCalendar $other): bool {}
function intlcal_is_weekend(IntlCalendar $calendar, ?float $timestamp = NULL): bool {}
function intlcal_set_first_day_of_week(IntlCalendar $calendar, int $dayOfWeek): bool {}
function intlcal_set_lenient(IntlCalendar $calendar, bool $lenient): bool {}
function intlcal_get_repeated_wall_time_option(IntlCalendar $calendar): int {}
function intlcal_equals(IntlCalendar $calendar, IntlCalendar $other): bool {}
function intlcal_get_skipped_wall_time_option(IntlCalendar $calendar): int {}
function intlcal_set_repeated_wall_time_option(IntlCalendar $calendar, int $option): bool {}
function intlcal_set_skipped_wall_time_option(IntlCalendar $calendar, int $option): bool {}
function intlcal_from_date_time(DateTime|string $datetime, ?string $locale = NULL): ?IntlCalendar {}
function intlcal_to_date_time(IntlCalendar $calendar): DateTime|false {}
function intlcal_get_error_code(IntlCalendar $calendar): int|false {}
function intlcal_get_error_message(IntlCalendar $calendar): string|false {}
function intlgregcal_create_instance($timezoneOrYear, $localeOrMonth, $day, $hour, $minute, $second): ?IntlGregorianCalendar {}
function intlgregcal_set_gregorian_change(IntlGregorianCalendar $calendar, float $timestamp): bool {}
function intlgregcal_get_gregorian_change(IntlGregorianCalendar $calendar): float {}
function intlgregcal_is_leap_year(IntlGregorianCalendar $calendar, int $year): bool {}
function collator_create(string $locale): ?Collator {}
function collator_compare(Collator $object, string $string1, string $string2): int|false {}
function collator_get_attribute(Collator $object, int $attribute): int|false {}
function collator_set_attribute(Collator $object, int $attribute, int $value): bool {}
function collator_get_strength(Collator $object): int {}
function collator_set_strength(Collator $object, int $strength): bool {}
function collator_sort(Collator $object, array $array, int $flags = 0): bool {}
function collator_sort_with_sort_keys(Collator $object, array $array): bool {}
function collator_asort(Collator $object, array $array, int $flags = 0): bool {}
function collator_get_locale(Collator $object, int $type): string|false {}
function collator_get_error_code(Collator $object): int|false {}
function collator_get_error_message(Collator $object): string|false {}
function collator_get_sort_key(Collator $object, string $string): string|false {}
function intl_get_error_code(): int {}
function intl_get_error_message(): string {}
function intl_is_failure(int $errorCode): bool {}
function intl_error_name(int $errorCode): string {}
function datefmt_create(?string $locale, int $dateType = 0, int $timeType = 0, $timezone = NULL, IntlCalendar|int|null $calendar = NULL, ?string $pattern = NULL): ?IntlDateFormatter {}
function datefmt_get_datetype(IntlDateFormatter $formatter): int|false {}
function datefmt_get_timetype(IntlDateFormatter $formatter): int|false {}
function datefmt_get_calendar(IntlDateFormatter $formatter): int|false {}
function datefmt_set_calendar(IntlDateFormatter $formatter, IntlCalendar|int|null $calendar): bool {}
function datefmt_get_timezone_id(IntlDateFormatter $formatter): string|false {}
function datefmt_get_calendar_object(IntlDateFormatter $formatter): IntlCalendar|false|null {}
function datefmt_get_timezone(IntlDateFormatter $formatter): IntlTimeZone|false {}
function datefmt_set_timezone(IntlDateFormatter $formatter, $timezone): ?bool {}
function datefmt_set_pattern(IntlDateFormatter $formatter, string $pattern): bool {}
function datefmt_get_pattern(IntlDateFormatter $formatter): string|false {}
function datefmt_get_locale(IntlDateFormatter $formatter, int $type = 0): string|false {}
function datefmt_set_lenient(IntlDateFormatter $formatter, bool $lenient): void {}
function datefmt_is_lenient(IntlDateFormatter $formatter): bool {}
function datefmt_format(IntlDateFormatter $formatter, $datetime): string|false {}
function datefmt_format_object($datetime, $format = NULL, ?string $locale = NULL): string|false {}
function datefmt_parse(IntlDateFormatter $formatter, string $string, $offset = NULL): int|float|false {}
function datefmt_localtime(IntlDateFormatter $formatter, string $string, $offset = NULL): array|false {}
function datefmt_get_error_code(IntlDateFormatter $formatter): int {}
function datefmt_get_error_message(IntlDateFormatter $formatter): string {}
function numfmt_create(string $locale, int $style, ?string $pattern = NULL): ?NumberFormatter {}
function numfmt_format(NumberFormatter $formatter, int|float $num, int $type = 0): string|false {}
function numfmt_parse(NumberFormatter $formatter, string $string, int $type = 3, $offset = NULL): int|float|false {}
function numfmt_format_currency(NumberFormatter $formatter, float $amount, string $currency): string|false {}
function numfmt_parse_currency(NumberFormatter $formatter, string $string, $currency, $offset = NULL): float|false {}
function numfmt_set_attribute(NumberFormatter $formatter, int $attribute, int|float $value): bool {}
function numfmt_get_attribute(NumberFormatter $formatter, int $attribute): int|float|false {}
function numfmt_set_text_attribute(NumberFormatter $formatter, int $attribute, string $value): bool {}
function numfmt_get_text_attribute(NumberFormatter $formatter, int $attribute): string|false {}
function numfmt_set_symbol(NumberFormatter $formatter, int $symbol, string $value): bool {}
function numfmt_get_symbol(NumberFormatter $formatter, int $symbol): string|false {}
function numfmt_set_pattern(NumberFormatter $formatter, string $pattern): bool {}
function numfmt_get_pattern(NumberFormatter $formatter): string|false {}
function numfmt_get_locale(NumberFormatter $formatter, int $type = 0): string|false {}
function numfmt_get_error_code(NumberFormatter $formatter): int {}
function numfmt_get_error_message(NumberFormatter $formatter): string {}
function grapheme_strlen(string $string): int|false|null {}
function grapheme_strpos(string $haystack, string $needle, int $offset = 0): int|false {}
function grapheme_stripos(string $haystack, string $needle, int $offset = 0): int|false {}
function grapheme_strrpos(string $haystack, string $needle, int $offset = 0): int|false {}
function grapheme_strripos(string $haystack, string $needle, int $offset = 0): int|false {}
function grapheme_substr(string $string, int $offset, ?int $length = NULL): string|false {}
function grapheme_strstr(string $haystack, string $needle, bool $beforeNeedle = false): string|false {}
function grapheme_stristr(string $haystack, string $needle, bool $beforeNeedle = false): string|false {}
function grapheme_extract(string $haystack, int $size, int $type = 0, int $offset = 0, $next = NULL): string|false {}
function idn_to_ascii(string $domain, int $flags = 0, int $variant = 1, $idna_info = NULL): string|false {}
function idn_to_utf8(string $domain, int $flags = 0, int $variant = 1, $idna_info = NULL): string|false {}
function locale_get_default(): string {}
function locale_set_default(string $locale): bool {}
function locale_get_primary_language(string $locale): ?string {}
function locale_get_script(string $locale): ?string {}
function locale_get_region(string $locale): ?string {}
function locale_get_keywords(string $locale): array|false|null {}
function locale_get_display_script(string $locale, ?string $displayLocale = NULL): string|false {}
function locale_get_display_region(string $locale, ?string $displayLocale = NULL): string|false {}
function locale_get_display_name(string $locale, ?string $displayLocale = NULL): string|false {}
function locale_get_display_language(string $locale, ?string $displayLocale = NULL): string|false {}
function locale_get_display_variant(string $locale, ?string $displayLocale = NULL): string|false {}
function locale_compose(array $subtags): string|false {}
function locale_parse(string $locale): ?array {}
function locale_get_all_variants(string $locale): ?array {}
function locale_filter_matches(string $languageTag, string $locale, bool $canonicalize = false): ?bool {}
function locale_canonicalize(string $locale): ?string {}
function locale_lookup(array $languageTag, string $locale, bool $canonicalize = false, ?string $defaultLocale = NULL): ?string {}
function locale_accept_from_http(string $header): string|false {}
function msgfmt_create(string $locale, string $pattern): ?MessageFormatter {}
function msgfmt_format(MessageFormatter $formatter, array $values): string|false {}
function msgfmt_format_message(string $locale, string $pattern, array $values): string|false {}
function msgfmt_parse(MessageFormatter $formatter, string $string): array|false {}
function msgfmt_parse_message(string $locale, string $pattern, string $message): array|false {}
function msgfmt_set_pattern(MessageFormatter $formatter, string $pattern): bool {}
function msgfmt_get_pattern(MessageFormatter $formatter): string|false {}
function msgfmt_get_locale(MessageFormatter $formatter): string {}
function msgfmt_get_error_code(MessageFormatter $formatter): int {}
function msgfmt_get_error_message(MessageFormatter $formatter): string {}
function normalizer_normalize(string $string, int $form = 16): string|false {}
function normalizer_is_normalized(string $string, int $form = 16): bool {}
function normalizer_get_raw_decomposition(string $string, int $form = 16): ?string {}
function resourcebundle_create(?string $locale, ?string $bundle, bool $fallback = true): ?ResourceBundle {}
function resourcebundle_get(ResourceBundle $bundle, $index, bool $fallback = true): mixed {}
function resourcebundle_count(ResourceBundle $bundle): int {}
function resourcebundle_locales(string $bundle): array|false {}
function resourcebundle_get_error_code(ResourceBundle $bundle): int {}
function resourcebundle_get_error_message(ResourceBundle $bundle): string {}
function intltz_count_equivalent_ids(string $timezoneId): int|false {}
function intltz_create_default(): IntlTimeZone {}
function intltz_create_enumeration($countryOrRawOffset = NULL): IntlIterator|false {}
function intltz_create_time_zone(string $timezoneId): ?IntlTimeZone {}
function intltz_create_time_zone_id_enumeration(int $type, ?string $region = NULL, ?int $rawOffset = NULL): IntlIterator|false {}
function intltz_from_date_time_zone(DateTimeZone $timezone): ?IntlTimeZone {}
function intltz_get_canonical_id(string $timezoneId, $isSystemId = NULL): string|false {}
function intltz_get_display_name(IntlTimeZone $timezone, bool $dst = false, int $style = 2, ?string $locale = NULL): string|false {}
function intltz_get_dst_savings(IntlTimeZone $timezone): int {}
function intltz_get_equivalent_id(string $timezoneId, int $offset): string|false {}
function intltz_get_error_code(IntlTimeZone $timezone): int|false {}
function intltz_get_error_message(IntlTimeZone $timezone): string|false {}
function intltz_get_gmt(): IntlTimeZone {}
function intltz_get_id(IntlTimeZone $timezone): string|false {}
function intltz_get_offset(IntlTimeZone $timezone, float $timestamp, bool $local, $rawOffset, $dstOffset): bool {}
function intltz_get_raw_offset(IntlTimeZone $timezone): int {}
function intltz_get_region(string $timezoneId): string|false {}
function intltz_get_tz_data_version(): string|false {}
function intltz_get_unknown(): IntlTimeZone {}
function intltz_get_windows_id(string $timezoneId): string|false {}
function intltz_get_id_for_windows_id(string $timezoneId, ?string $region = NULL): string|false {}
function intltz_has_same_rules(IntlTimeZone $timezone, IntlTimeZone $other): bool {}
function intltz_to_date_time_zone(IntlTimeZone $timezone): DateTimeZone|false {}
function intltz_use_daylight_time(IntlTimeZone $timezone): bool {}
function transliterator_create(string $id, int $direction = 0): ?Transliterator {}
function transliterator_create_from_rules(string $rules, int $direction = 0): ?Transliterator {}
function transliterator_list_ids(): array|false {}
function transliterator_create_inverse(Transliterator $transliterator): ?Transliterator {}
function transliterator_transliterate(Transliterator|string $transliterator, string $string, int $start = 0, int $end = -1): string|false {}
function transliterator_get_error_code(Transliterator $transliterator): int|false {}
function transliterator_get_error_message(Transliterator $transliterator): string|false {}
function mb_language(?string $language = NULL): string|bool {}
function mb_internal_encoding(?string $encoding = NULL): string|bool {}
function mb_http_input(?string $type = NULL): array|string|false {}
function mb_http_output(?string $encoding = NULL): string|bool {}
function mb_detect_order(array|string|null $encoding = NULL): array|bool {}
function mb_substitute_character(string|int|null $substitute_character = NULL): string|int|bool {}
function mb_preferred_mime_name(string $encoding): string|false {}
function mb_parse_str(string $string, $result): bool {}
function mb_output_handler(string $string, int $status): string {}
function mb_str_split(string $string, int $length = 1, ?string $encoding = NULL): array {}
function mb_strlen(string $string, ?string $encoding = NULL): int {}
function mb_strpos(string $haystack, string $needle, int $offset = 0, ?string $encoding = NULL): int|false {}
function mb_strrpos(string $haystack, string $needle, int $offset = 0, ?string $encoding = NULL): int|false {}
function mb_stripos(string $haystack, string $needle, int $offset = 0, ?string $encoding = NULL): int|false {}
function mb_strripos(string $haystack, string $needle, int $offset = 0, ?string $encoding = NULL): int|false {}
function mb_strstr(string $haystack, string $needle, bool $before_needle = false, ?string $encoding = NULL): string|false {}
function mb_strrchr(string $haystack, string $needle, bool $before_needle = false, ?string $encoding = NULL): string|false {}
function mb_stristr(string $haystack, string $needle, bool $before_needle = false, ?string $encoding = NULL): string|false {}
function mb_strrichr(string $haystack, string $needle, bool $before_needle = false, ?string $encoding = NULL): string|false {}
function mb_substr_count(string $haystack, string $needle, ?string $encoding = NULL): int {}
function mb_substr(string $string, int $start, ?int $length = NULL, ?string $encoding = NULL): string {}
function mb_strcut(string $string, int $start, ?int $length = NULL, ?string $encoding = NULL): string {}
function mb_strwidth(string $string, ?string $encoding = NULL): int {}
function mb_strimwidth(string $string, int $start, int $width, string $trim_marker = '', ?string $encoding = NULL): string {}
function mb_convert_encoding(array|string $string, string $to_encoding, array|string|null $from_encoding = NULL): array|string|false {}
function mb_convert_case(string $string, int $mode, ?string $encoding = NULL): string {}
function mb_strtoupper(string $string, ?string $encoding = NULL): string {}
function mb_strtolower(string $string, ?string $encoding = NULL): string {}
function mb_detect_encoding(string $string, array|string|null $encodings = NULL, bool $strict = false): string|false {}
function mb_list_encodings(): array {}
function mb_encoding_aliases(string $encoding): array {}
function mb_encode_mimeheader(string $string, ?string $charset = NULL, ?string $transfer_encoding = NULL, string $newline = '
', int $indent = 0): string {}
function mb_decode_mimeheader(string $string): string {}
function mb_convert_kana(string $string, string $mode = 'KV', ?string $encoding = NULL): string {}
function mb_convert_variables(string $to_encoding, array|string $from_encoding, mixed $var, mixed $vars): string|false {}
function mb_encode_numericentity(string $string, array $map, ?string $encoding = NULL, bool $hex = false): string {}
function mb_decode_numericentity(string $string, array $map, ?string $encoding = NULL): string {}
function mb_send_mail(string $to, string $subject, string $message, array|string $additional_headers = array (
), ?string $additional_params = NULL): bool {}
function mb_get_info(string $type = 'all'): array|string|int|false {}
function mb_check_encoding(array|string|null $value = NULL, ?string $encoding = NULL): bool {}
function mb_scrub(string $string, ?string $encoding = NULL): string {}
function mb_ord(string $string, ?string $encoding = NULL): int|false {}
function mb_chr(int $codepoint, ?string $encoding = NULL): string|false {}
function mb_regex_encoding(?string $encoding = NULL): string|bool {}
function mb_ereg(string $pattern, string $string, $matches = NULL): bool {}
function mb_eregi(string $pattern, string $string, $matches = NULL): bool {}
function mb_ereg_replace(string $pattern, string $replacement, string $string, ?string $options = NULL): string|false|null {}
function mb_eregi_replace(string $pattern, string $replacement, string $string, ?string $options = NULL): string|false|null {}
function mb_ereg_replace_callback(string $pattern, callable $callback, string $string, ?string $options = NULL): string|false|null {}
function mb_split(string $pattern, string $string, int $limit = -1): array|false {}
function mb_ereg_match(string $pattern, string $string, ?string $options = NULL): bool {}
function mb_ereg_search(?string $pattern = NULL, ?string $options = NULL): bool {}
function mb_ereg_search_pos(?string $pattern = NULL, ?string $options = NULL): array|false {}
function mb_ereg_search_regs(?string $pattern = NULL, ?string $options = NULL): array|false {}
function mb_ereg_search_init(string $string, ?string $pattern = NULL, ?string $options = NULL): bool {}
function mb_ereg_search_getregs(): array|false {}
function mb_ereg_search_getpos(): int {}
function mb_ereg_search_setpos(int $offset): bool {}
function mb_regex_set_options(?string $options = NULL): string {}
function msgpack_serialize($value) {}
function msgpack_unserialize($str, $object) {}
function msgpack_pack($value) {}
function msgpack_unpack($str, $object) {}
function mysqli_affected_rows(mysqli $mysql): string|int {}
function mysqli_autocommit(mysqli $mysql, bool $enable): bool {}
function mysqli_begin_transaction(mysqli $mysql, int $flags = 0, ?string $name = NULL): bool {}
function mysqli_change_user(mysqli $mysql, string $username, string $password, ?string $database): bool {}
function mysqli_character_set_name(mysqli $mysql): string {}
function mysqli_close(mysqli $mysql): true {}
function mysqli_commit(mysqli $mysql, int $flags = 0, ?string $name = NULL): bool {}
function mysqli_connect(?string $hostname = NULL, ?string $username = NULL, ?string $password = NULL, ?string $database = NULL, ?int $port = NULL, ?string $socket = NULL): mysqli|false {}
function mysqli_connect_errno(): int {}
function mysqli_connect_error(): ?string {}
function mysqli_data_seek(mysqli_result $result, int $offset): bool {}
function mysqli_dump_debug_info(mysqli $mysql): bool {}
function mysqli_debug(string $options): true {}
function mysqli_errno(mysqli $mysql): int {}
function mysqli_error(mysqli $mysql): string {}
function mysqli_error_list(mysqli $mysql): array {}
function mysqli_stmt_execute(mysqli_stmt $statement, ?array $params = NULL): bool {}
function mysqli_execute(mysqli_stmt $statement, ?array $params = NULL): bool {}
function mysqli_execute_query(mysqli $mysql, string $query, ?array $params = NULL): mysqli_result|bool {}
function mysqli_fetch_field(mysqli_result $result): object|false {}
function mysqli_fetch_fields(mysqli_result $result): array {}
function mysqli_fetch_field_direct(mysqli_result $result, int $index): object|false {}
function mysqli_fetch_lengths(mysqli_result $result): array|false {}
function mysqli_fetch_all(mysqli_result $result, int $mode = 2): array {}
function mysqli_fetch_array(mysqli_result $result, int $mode = 3): array|false|null {}
function mysqli_fetch_assoc(mysqli_result $result): array|false|null {}
function mysqli_fetch_object(mysqli_result $result, string $class = 'stdClass', array $constructor_args = array (
)): object|false|null {}
function mysqli_fetch_row(mysqli_result $result): array|false|null {}
function mysqli_fetch_column(mysqli_result $result, int $column = 0): string|int|float|false|null {}
function mysqli_field_count(mysqli $mysql): int {}
function mysqli_field_seek(mysqli_result $result, int $index): bool {}
function mysqli_field_tell(mysqli_result $result): int {}
function mysqli_free_result(mysqli_result $result): void {}
function mysqli_get_connection_stats(mysqli $mysql): array {}
function mysqli_get_client_stats(): array {}
function mysqli_get_charset(mysqli $mysql): ?object {}
function mysqli_get_client_info(?mysqli $mysql = NULL): string {}
function mysqli_get_client_version(): int {}
function mysqli_get_links_stats(): array {}
function mysqli_get_host_info(mysqli $mysql): string {}
function mysqli_get_proto_info(mysqli $mysql): int {}
function mysqli_get_server_info(mysqli $mysql): string {}
function mysqli_get_server_version(mysqli $mysql): int {}
function mysqli_get_warnings(mysqli $mysql): mysqli_warning|false {}
function mysqli_init(): mysqli|false {}
function mysqli_info(mysqli $mysql): ?string {}
function mysqli_insert_id(mysqli $mysql): string|int {}
function mysqli_kill(mysqli $mysql, int $process_id): bool {}
function mysqli_more_results(mysqli $mysql): bool {}
function mysqli_multi_query(mysqli $mysql, string $query): bool {}
function mysqli_next_result(mysqli $mysql): bool {}
function mysqli_num_fields(mysqli_result $result): int {}
function mysqli_num_rows(mysqli_result $result): string|int {}
function mysqli_options(mysqli $mysql, int $option, $value): bool {}
function mysqli_set_opt(mysqli $mysql, int $option, $value): bool {}
function mysqli_ping(mysqli $mysql): bool {}
function mysqli_poll(?array $read, ?array $error, array $reject, int $seconds, int $microseconds = 0): int|false {}
function mysqli_prepare(mysqli $mysql, string $query): mysqli_stmt|false {}
function mysqli_report(int $flags): bool {}
function mysqli_query(mysqli $mysql, string $query, int $result_mode = 0): mysqli_result|bool {}
function mysqli_real_connect(mysqli $mysql, ?string $hostname = NULL, ?string $username = NULL, ?string $password = NULL, ?string $database = NULL, ?int $port = NULL, ?string $socket = NULL, int $flags = 0): bool {}
function mysqli_real_escape_string(mysqli $mysql, string $string): string {}
function mysqli_escape_string(mysqli $mysql, string $string): string {}
function mysqli_real_query(mysqli $mysql, string $query): bool {}
function mysqli_reap_async_query(mysqli $mysql): mysqli_result|bool {}
function mysqli_release_savepoint(mysqli $mysql, string $name): bool {}
function mysqli_rollback(mysqli $mysql, int $flags = 0, ?string $name = NULL): bool {}
function mysqli_savepoint(mysqli $mysql, string $name): bool {}
function mysqli_select_db(mysqli $mysql, string $database): bool {}
function mysqli_set_charset(mysqli $mysql, string $charset): bool {}
function mysqli_stmt_affected_rows(mysqli_stmt $statement): string|int {}
function mysqli_stmt_attr_get(mysqli_stmt $statement, int $attribute): int {}
function mysqli_stmt_attr_set(mysqli_stmt $statement, int $attribute, int $value): bool {}
function mysqli_stmt_bind_param(mysqli_stmt $statement, string $types, mixed $vars): bool {}
function mysqli_stmt_bind_result(mysqli_stmt $statement, mixed $vars): bool {}
function mysqli_stmt_close(mysqli_stmt $statement): true {}
function mysqli_stmt_data_seek(mysqli_stmt $statement, int $offset): void {}
function mysqli_stmt_errno(mysqli_stmt $statement): int {}
function mysqli_stmt_error(mysqli_stmt $statement): string {}
function mysqli_stmt_error_list(mysqli_stmt $statement): array {}
function mysqli_stmt_fetch(mysqli_stmt $statement): ?bool {}
function mysqli_stmt_field_count(mysqli_stmt $statement): int {}
function mysqli_stmt_free_result(mysqli_stmt $statement): void {}
function mysqli_stmt_get_result(mysqli_stmt $statement): mysqli_result|false {}
function mysqli_stmt_get_warnings(mysqli_stmt $statement): mysqli_warning|false {}
function mysqli_stmt_init(mysqli $mysql): mysqli_stmt|false {}
function mysqli_stmt_insert_id(mysqli_stmt $statement): string|int {}
function mysqli_stmt_more_results(mysqli_stmt $statement): bool {}
function mysqli_stmt_next_result(mysqli_stmt $statement): bool {}
function mysqli_stmt_num_rows(mysqli_stmt $statement): string|int {}
function mysqli_stmt_param_count(mysqli_stmt $statement): int {}
function mysqli_stmt_prepare(mysqli_stmt $statement, string $query): bool {}
function mysqli_stmt_reset(mysqli_stmt $statement): bool {}
function mysqli_stmt_result_metadata(mysqli_stmt $statement): mysqli_result|false {}
function mysqli_stmt_send_long_data(mysqli_stmt $statement, int $param_num, string $data): bool {}
function mysqli_stmt_store_result(mysqli_stmt $statement): bool {}
function mysqli_stmt_sqlstate(mysqli_stmt $statement): string {}
function mysqli_sqlstate(mysqli $mysql): string {}
function mysqli_ssl_set(mysqli $mysql, ?string $key, ?string $certificate, ?string $ca_certificate, ?string $ca_path, ?string $cipher_algos): true {}
function mysqli_stat(mysqli $mysql): string|false {}
function mysqli_store_result(mysqli $mysql, int $mode = 0): mysqli_result|false {}
function mysqli_thread_id(mysqli $mysql): int {}
function mysqli_thread_safe(): bool {}
function mysqli_use_result(mysqli $mysql): mysqli_result|false {}
function mysqli_warning_count(mysqli $mysql): int {}
function mysqli_refresh(mysqli $mysql, int $flags): bool {}
function odbc_close_all(): void {}
function odbc_binmode($statement, int $mode): bool {}
function odbc_longreadlen($statement, int $length): bool {}
function odbc_prepare($odbc, string $query) {}
function odbc_execute($statement, array $params = array (
)): bool {}
function odbc_cursor($statement): string|false {}
function odbc_data_source($odbc, int $fetch_type): array|false {}
function odbc_exec($odbc, string $query) {}
function odbc_do($odbc, string $query) {}
function odbc_fetch_object($statement, int $row = -1): stdClass|false {}
function odbc_fetch_array($statement, int $row = -1): array|false {}
function odbc_fetch_into($statement, $array, int $row = 0): int|false {}
function odbc_fetch_row($statement, ?int $row = NULL): bool {}
function odbc_result($statement, string|int $field): string|bool|null {}
function odbc_result_all($statement, string $format = ''): int|false {}
function odbc_free_result($statement): bool {}
function odbc_connect(string $dsn, string $user, string $password, int $cursor_option = 2) {}
function odbc_pconnect(string $dsn, string $user, string $password, int $cursor_option = 2) {}
function odbc_close($odbc): void {}
function odbc_num_rows($statement): int {}
function odbc_next_result($statement): bool {}
function odbc_num_fields($statement): int {}
function odbc_field_name($statement, int $field): string|false {}
function odbc_field_type($statement, int $field): string|false {}
function odbc_field_len($statement, int $field): int|false {}
function odbc_field_precision($statement, int $field): int|false {}
function odbc_field_scale($statement, int $field): int|false {}
function odbc_field_num($statement, string $field): int|false {}
function odbc_autocommit($odbc, bool $enable = false): int|bool {}
function odbc_commit($odbc): bool {}
function odbc_rollback($odbc): bool {}
function odbc_error($odbc = NULL): string {}
function odbc_errormsg($odbc = NULL): string {}
function odbc_setoption($odbc, int $which, int $option, int $value): bool {}
function odbc_tables($odbc, ?string $catalog = NULL, ?string $schema = NULL, ?string $table = NULL, ?string $types = NULL) {}
function odbc_columns($odbc, ?string $catalog = NULL, ?string $schema = NULL, ?string $table = NULL, ?string $column = NULL) {}
function odbc_gettypeinfo($odbc, int $data_type = 0) {}
function odbc_primarykeys($odbc, ?string $catalog, string $schema, string $table) {}
function odbc_procedurecolumns($odbc, ?string $catalog = NULL, ?string $schema = NULL, ?string $procedure = NULL, ?string $column = NULL) {}
function odbc_procedures($odbc, ?string $catalog = NULL, ?string $schema = NULL, ?string $procedure = NULL) {}
function odbc_foreignkeys($odbc, ?string $pk_catalog, string $pk_schema, string $pk_table, string $fk_catalog, string $fk_schema, string $fk_table) {}
function odbc_specialcolumns($odbc, int $type, ?string $catalog, string $schema, string $table, int $scope, int $nullable) {}
function odbc_statistics($odbc, ?string $catalog, string $schema, string $table, int $unique, int $accuracy) {}
function odbc_tableprivileges($odbc, ?string $catalog, string $schema, string $table) {}
function odbc_columnprivileges($odbc, ?string $catalog, string $schema, string $table, string $column) {}
function odbc_connection_string_is_quoted(string $str): bool {}
function odbc_connection_string_should_quote(string $str): bool {}
function odbc_connection_string_quote(string $str): string {}
function pg_connect(string $connection_string, int $flags = 0): PgSql\Connection|false {}
function pg_pconnect(string $connection_string, int $flags = 0): PgSql\Connection|false {}
function pg_connect_poll(PgSql\Connection $connection): int {}
function pg_close(?PgSql\Connection $connection = NULL): bool {}
function pg_dbname(?PgSql\Connection $connection = NULL): string {}
function pg_last_error(?PgSql\Connection $connection = NULL): string {}
function pg_errormessage(?PgSql\Connection $connection = NULL): string {}
function pg_options(?PgSql\Connection $connection = NULL): string {}
function pg_port(?PgSql\Connection $connection = NULL): string {}
function pg_tty(?PgSql\Connection $connection = NULL): string {}
function pg_host(?PgSql\Connection $connection = NULL): string {}
function pg_version(?PgSql\Connection $connection = NULL): array {}
function pg_parameter_status($connection, string $name): string|false {}
function pg_ping(?PgSql\Connection $connection = NULL): bool {}
function pg_query($connection, string $query): PgSql\Result|false {}
function pg_exec($connection, string $query): PgSql\Result|false {}
function pg_query_params($connection, $query, array $params): PgSql\Result|false {}
function pg_prepare($connection, string $statement_name, string $query): PgSql\Result|false {}
function pg_execute($connection, $statement_name, array $params): PgSql\Result|false {}
function pg_num_rows(PgSql\Result $result): int {}
function pg_numrows(PgSql\Result $result): int {}
function pg_num_fields(PgSql\Result $result): int {}
function pg_numfields(PgSql\Result $result): int {}
function pg_affected_rows(PgSql\Result $result): int {}
function pg_cmdtuples(PgSql\Result $result): int {}
function pg_last_notice(PgSql\Connection $connection, int $mode = 1): array|string|bool {}
function pg_field_table(PgSql\Result $result, int $field, bool $oid_only = false): string|int|false {}
function pg_field_name(PgSql\Result $result, int $field): string {}
function pg_fieldname(PgSql\Result $result, int $field): string {}
function pg_field_size(PgSql\Result $result, int $field): int {}
function pg_fieldsize(PgSql\Result $result, int $field): int {}
function pg_field_type(PgSql\Result $result, int $field): string {}
function pg_fieldtype(PgSql\Result $result, int $field): string {}
function pg_field_type_oid(PgSql\Result $result, int $field): string|int {}
function pg_field_num(PgSql\Result $result, string $field): int {}
function pg_fieldnum(PgSql\Result $result, string $field): int {}
function pg_fetch_result(PgSql\Result $result, $row, string|int $field): string|false|null {}
function pg_result(PgSql\Result $result, $row, string|int $field): string|false|null {}
function pg_fetch_row(PgSql\Result $result, ?int $row = NULL, int $mode = 2): array|false {}
function pg_fetch_assoc(PgSql\Result $result, ?int $row = NULL): array|false {}
function pg_fetch_array(PgSql\Result $result, ?int $row = NULL, int $mode = 3): array|false {}
function pg_fetch_object(PgSql\Result $result, ?int $row = NULL, string $class = 'stdClass', array $constructor_args = array (
)): object|false {}
function pg_fetch_all(PgSql\Result $result, int $mode = 1): array {}
function pg_fetch_all_columns(PgSql\Result $result, int $field = 0): array {}
function pg_result_seek(PgSql\Result $result, int $row): bool {}
function pg_field_prtlen(PgSql\Result $result, $row, string|int $field): int|false {}
function pg_fieldprtlen(PgSql\Result $result, $row, string|int $field): int|false {}
function pg_field_is_null(PgSql\Result $result, $row, string|int $field): int|false {}
function pg_fieldisnull(PgSql\Result $result, $row, string|int $field): int|false {}
function pg_free_result(PgSql\Result $result): bool {}
function pg_freeresult(PgSql\Result $result): bool {}
function pg_last_oid(PgSql\Result $result): string|int|false {}
function pg_getlastoid(PgSql\Result $result): string|int|false {}
function pg_trace(string $filename, string $mode = 'w', ?PgSql\Connection $connection = NULL): bool {}
function pg_untrace(?PgSql\Connection $connection = NULL): bool {}
function pg_lo_create($connection, $oid): string|int|false {}
function pg_locreate($connection, $oid): string|int|false {}
function pg_lo_unlink($connection, $oid): bool {}
function pg_lounlink($connection, $oid): bool {}
function pg_lo_open($connection, $oid, string $mode): PgSql\Lob|false {}
function pg_loopen($connection, $oid, string $mode): PgSql\Lob|false {}
function pg_lo_close(PgSql\Lob $lob): bool {}
function pg_loclose(PgSql\Lob $lob): bool {}
function pg_lo_read(PgSql\Lob $lob, int $length = 8192): string|false {}
function pg_loread(PgSql\Lob $lob, int $length = 8192): string|false {}
function pg_lo_write(PgSql\Lob $lob, string $data, ?int $length = NULL): int|false {}
function pg_lowrite(PgSql\Lob $lob, string $data, ?int $length = NULL): int|false {}
function pg_lo_read_all(PgSql\Lob $lob): int {}
function pg_loreadall(PgSql\Lob $lob): int {}
function pg_lo_import($connection, $filename, $oid): string|int|false {}
function pg_loimport($connection, $filename, $oid): string|int|false {}
function pg_lo_export($connection, $oid, $filename): bool {}
function pg_loexport($connection, $oid, $filename): bool {}
function pg_lo_seek(PgSql\Lob $lob, int $offset, int $whence = 1): bool {}
function pg_lo_tell(PgSql\Lob $lob): int {}
function pg_lo_truncate(PgSql\Lob $lob, int $size): bool {}
function pg_set_error_verbosity($connection, int $verbosity): int|false {}
function pg_set_client_encoding($connection, string $encoding): int {}
function pg_setclientencoding($connection, string $encoding): int {}
function pg_client_encoding(?PgSql\Connection $connection = NULL): string {}
function pg_clientencoding(?PgSql\Connection $connection = NULL): string {}
function pg_end_copy(?PgSql\Connection $connection = NULL): bool {}
function pg_put_line($connection, string $query): bool {}
function pg_copy_to(PgSql\Connection $connection, string $table_name, string $separator = '	', string $null_as = '\\\\N'): array|false {}
function pg_copy_from(PgSql\Connection $connection, string $table_name, array $rows, string $separator = '	', string $null_as = '\\\\N'): bool {}
function pg_escape_string($connection, string $string): string {}
function pg_escape_bytea($connection, string $string): string {}
function pg_unescape_bytea(string $string): string {}
function pg_escape_literal($connection, string $string): string|false {}
function pg_escape_identifier($connection, string $string): string|false {}
function pg_result_error(PgSql\Result $result): string|false {}
function pg_result_error_field(PgSql\Result $result, int $field_code): string|false|null {}
function pg_connection_status(PgSql\Connection $connection): int {}
function pg_transaction_status(PgSql\Connection $connection): int {}
function pg_connection_reset(PgSql\Connection $connection): bool {}
function pg_cancel_query(PgSql\Connection $connection): bool {}
function pg_connection_busy(PgSql\Connection $connection): bool {}
function pg_send_query(PgSql\Connection $connection, string $query): int|bool {}
function pg_send_query_params(PgSql\Connection $connection, string $query, array $params): int|bool {}
function pg_send_prepare(PgSql\Connection $connection, string $statement_name, string $query): int|bool {}
function pg_send_execute(PgSql\Connection $connection, string $statement_name, array $params): int|bool {}
function pg_get_result(PgSql\Connection $connection): PgSql\Result|false {}
function pg_result_status(PgSql\Result $result, int $mode = 1): string|int {}
function pg_get_notify(PgSql\Connection $connection, int $mode = 1): array|false {}
function pg_get_pid(PgSql\Connection $connection): int {}
function pg_socket(PgSql\Connection $connection) {}
function pg_consume_input(PgSql\Connection $connection): bool {}
function pg_flush(PgSql\Connection $connection): int|bool {}
function pg_meta_data(PgSql\Connection $connection, string $table_name, bool $extended = false): array|false {}
function pg_convert(PgSql\Connection $connection, string $table_name, array $values, int $flags = 0): array|false {}
function pg_insert(PgSql\Connection $connection, string $table_name, array $values, int $flags = 512): PgSql\Result|string|bool {}
function pg_update(PgSql\Connection $connection, string $table_name, array $values, array $conditions, int $flags = 512): string|bool {}
function pg_delete(PgSql\Connection $connection, string $table_name, array $conditions, int $flags = 512): string|bool {}
function pg_select(PgSql\Connection $connection, string $table_name, array $conditions, int $flags = 512, int $mode = 1): array|string|false {}
function posix_kill(int $process_id, int $signal): bool {}
function posix_getpid(): int {}
function posix_getppid(): int {}
function posix_getuid(): int {}
function posix_setuid(int $user_id): bool {}
function posix_geteuid(): int {}
function posix_seteuid(int $user_id): bool {}
function posix_getgid(): int {}
function posix_setgid(int $group_id): bool {}
function posix_getegid(): int {}
function posix_setegid(int $group_id): bool {}
function posix_getgroups(): array|false {}
function posix_getlogin(): string|false {}
function posix_getpgrp(): int {}
function posix_setsid(): int {}
function posix_setpgid(int $process_id, int $process_group_id): bool {}
function posix_getpgid(int $process_id): int|false {}
function posix_getsid(int $process_id): int|false {}
function posix_uname(): array|false {}
function posix_times(): array|false {}
function posix_ctermid(): string|false {}
function posix_ttyname($file_descriptor): string|false {}
function posix_isatty($file_descriptor): bool {}
function posix_getcwd(): string|false {}
function posix_mkfifo(string $filename, int $permissions): bool {}
function posix_mknod(string $filename, int $flags, int $major = 0, int $minor = 0): bool {}
function posix_access(string $filename, int $flags = 0): bool {}
function posix_getgrnam(string $name): array|false {}
function posix_getgrgid(int $group_id): array|false {}
function posix_getpwnam(string $username): array|false {}
function posix_getpwuid(int $user_id): array|false {}
function posix_getrlimit(): array|false {}
function posix_setrlimit(int $resource, int $soft_limit, int $hard_limit): bool {}
function posix_get_last_error(): int {}
function posix_errno(): int {}
function posix_strerror(int $error_code): string {}
function posix_initgroups(string $username, int $group_id): bool {}
function readline(?string $prompt = NULL): string|false {}
function readline_info(?string $var_name = NULL, $value = NULL): mixed {}
function readline_add_history(string $prompt): bool {}
function readline_clear_history(): bool {}
function readline_list_history(): array {}
function readline_read_history(?string $filename = NULL): bool {}
function readline_write_history(?string $filename = NULL): bool {}
function readline_completion_function(callable $callback): bool {}
function readline_callback_handler_install(string $prompt, callable $callback): bool {}
function readline_callback_read_char(): void {}
function readline_callback_handler_remove(): bool {}
function readline_redisplay(): void {}
function readline_on_new_line(): void {}
function shmop_open(int $key, string $mode, int $permissions, int $size): Shmop|false {}
function shmop_read(Shmop $shmop, int $offset, int $size): string {}
function shmop_close(Shmop $shmop): void {}
function shmop_size(Shmop $shmop): int {}
function shmop_write(Shmop $shmop, string $data, int $offset): int {}
function shmop_delete(Shmop $shmop): bool {}
function simplexml_load_file(string $filename, ?string $class_name = 'SimpleXMLElement', int $options = 0, string $namespace_or_prefix = '', bool $is_prefix = false): SimpleXMLElement|false {}
function simplexml_load_string(string $data, ?string $class_name = 'SimpleXMLElement', int $options = 0, string $namespace_or_prefix = '', bool $is_prefix = false): SimpleXMLElement|false {}
function simplexml_import_dom(SimpleXMLElement|DOMNode $node, ?string $class_name = 'SimpleXMLElement'): ?SimpleXMLElement {}
function socket_select(?array $read, ?array $write, ?array $except, ?int $seconds, int $microseconds = 0): int|false {}
function socket_create_listen(int $port, int $backlog = 128): Socket|false {}
function socket_accept(Socket $socket): Socket|false {}
function socket_set_nonblock(Socket $socket): bool {}
function socket_set_block(Socket $socket): bool {}
function socket_listen(Socket $socket, int $backlog = 0): bool {}
function socket_close(Socket $socket): void {}
function socket_write(Socket $socket, string $data, ?int $length = NULL): int|false {}
function socket_read(Socket $socket, int $length, int $mode = 2): string|false {}
function socket_getsockname(Socket $socket, $address, $port = NULL): bool {}
function socket_getpeername(Socket $socket, $address, $port = NULL): bool {}
function socket_create(int $domain, int $type, int $protocol): Socket|false {}
function socket_connect(Socket $socket, string $address, ?int $port = NULL): bool {}
function socket_strerror(int $error_code): string {}
function socket_bind(Socket $socket, string $address, int $port = 0): bool {}
function socket_recv(Socket $socket, $data, int $length, int $flags): int|false {}
function socket_send(Socket $socket, string $data, int $length, int $flags): int|false {}
function socket_recvfrom(Socket $socket, $data, int $length, int $flags, $address, $port = NULL): int|false {}
function socket_sendto(Socket $socket, string $data, int $length, int $flags, string $address, ?int $port = NULL): int|false {}
function socket_get_option(Socket $socket, int $level, int $option): array|int|false {}
function socket_getopt(Socket $socket, int $level, int $option): array|int|false {}
function socket_set_option(Socket $socket, int $level, int $option, $value): bool {}
function socket_setopt(Socket $socket, int $level, int $option, $value): bool {}
function socket_create_pair(int $domain, int $type, int $protocol, $pair): bool {}
function socket_shutdown(Socket $socket, int $mode = 2): bool {}
function socket_last_error(?Socket $socket = NULL): int {}
function socket_clear_error(?Socket $socket = NULL): void {}
function socket_import_stream($stream): Socket|false {}
function socket_export_stream(Socket $socket) {}
function socket_sendmsg(Socket $socket, array $message, int $flags = 0): int|false {}
function socket_recvmsg(Socket $socket, array $message, int $flags = 0): int|false {}
function socket_cmsg_space(int $level, int $type, int $num = 0): ?int {}
function socket_addrinfo_lookup(string $host, ?string $service = NULL, array $hints = array (
)): array|false {}
function socket_addrinfo_connect(AddressInfo $address): Socket|false {}
function socket_addrinfo_bind(AddressInfo $address): Socket|false {}
function socket_addrinfo_explain(AddressInfo $address): array {}
function msg_get_queue(int $key, int $permissions = 438): SysvMessageQueue|false {}
function msg_send(SysvMessageQueue $queue, int $message_type, $message, bool $serialize = true, bool $blocking = true, $error_code = NULL): bool {}
function msg_receive(SysvMessageQueue $queue, int $desired_message_type, $received_message_type, int $max_message_size, mixed $message, bool $unserialize = true, int $flags = 0, $error_code = NULL): bool {}
function msg_remove_queue(SysvMessageQueue $queue): bool {}
function msg_stat_queue(SysvMessageQueue $queue): array|false {}
function msg_set_queue(SysvMessageQueue $queue, array $data): bool {}
function msg_queue_exists(int $key): bool {}
function sem_get(int $key, int $max_acquire = 1, int $permissions = 438, bool $auto_release = true): SysvSemaphore|false {}
function sem_acquire(SysvSemaphore $semaphore, bool $non_blocking = false): bool {}
function sem_release(SysvSemaphore $semaphore): bool {}
function sem_remove(SysvSemaphore $semaphore): bool {}
function shm_attach(int $key, ?int $size = NULL, int $permissions = 438): SysvSharedMemory|false {}
function shm_detach(SysvSharedMemory $shm): bool {}
function shm_has_var(SysvSharedMemory $shm, int $key): bool {}
function shm_remove(SysvSharedMemory $shm): bool {}
function shm_put_var(SysvSharedMemory $shm, int $key, mixed $value): bool {}
function shm_get_var(SysvSharedMemory $shm, int $key): mixed {}
function shm_remove_var(SysvSharedMemory $shm, int $key): bool {}
function tidy_parse_string(string $string, array|string|null $config = NULL, ?string $encoding = NULL): tidy|false {}
function tidy_get_error_buffer(tidy $tidy): string|false {}
function tidy_get_output(tidy $tidy): string {}
function tidy_parse_file(string $filename, array|string|null $config = NULL, ?string $encoding = NULL, bool $useIncludePath = false): tidy|false {}
function tidy_clean_repair(tidy $tidy): bool {}
function tidy_repair_string(string $string, array|string|null $config = NULL, ?string $encoding = NULL): string|false {}
function tidy_repair_file(string $filename, array|string|null $config = NULL, ?string $encoding = NULL, bool $useIncludePath = false): string|false {}
function tidy_diagnose(tidy $tidy): bool {}
function tidy_get_release(): string {}
function tidy_get_opt_doc(tidy $tidy, string $option): string|false {}
function tidy_get_config(tidy $tidy): array {}
function tidy_get_status(tidy $tidy): int {}
function tidy_get_html_ver(tidy $tidy): int {}
function tidy_is_xhtml(tidy $tidy): bool {}
function tidy_is_xml(tidy $tidy): bool {}
function tidy_error_count(tidy $tidy): int {}
function tidy_warning_count(tidy $tidy): int {}
function tidy_access_count(tidy $tidy): int {}
function tidy_config_count(tidy $tidy): int {}
function tidy_getopt(tidy $tidy, string $option): string|int|bool {}
function tidy_get_root(tidy $tidy): ?tidyNode {}
function tidy_get_html(tidy $tidy): ?tidyNode {}
function tidy_get_head(tidy $tidy): ?tidyNode {}
function tidy_get_body(tidy $tidy): ?tidyNode {}
function token_get_all(string $code, int $flags = 0): array {}
function token_name(int $id): string {}
function xmlwriter_open_uri(string $uri): XMLWriter|false {}
function xmlwriter_open_memory(): XMLWriter|false {}
function xmlwriter_set_indent(XMLWriter $writer, bool $enable): bool {}
function xmlwriter_set_indent_string(XMLWriter $writer, string $indentation): bool {}
function xmlwriter_start_comment(XMLWriter $writer): bool {}
function xmlwriter_end_comment(XMLWriter $writer): bool {}
function xmlwriter_start_attribute(XMLWriter $writer, string $name): bool {}
function xmlwriter_end_attribute(XMLWriter $writer): bool {}
function xmlwriter_write_attribute(XMLWriter $writer, string $name, string $value): bool {}
function xmlwriter_start_attribute_ns(XMLWriter $writer, ?string $prefix, string $name, ?string $namespace): bool {}
function xmlwriter_write_attribute_ns(XMLWriter $writer, ?string $prefix, string $name, ?string $namespace, string $value): bool {}
function xmlwriter_start_element(XMLWriter $writer, string $name): bool {}
function xmlwriter_end_element(XMLWriter $writer): bool {}
function xmlwriter_full_end_element(XMLWriter $writer): bool {}
function xmlwriter_start_element_ns(XMLWriter $writer, ?string $prefix, string $name, ?string $namespace): bool {}
function xmlwriter_write_element(XMLWriter $writer, string $name, ?string $content = NULL): bool {}
function xmlwriter_write_element_ns(XMLWriter $writer, ?string $prefix, string $name, ?string $namespace, ?string $content = NULL): bool {}
function xmlwriter_start_pi(XMLWriter $writer, string $target): bool {}
function xmlwriter_end_pi(XMLWriter $writer): bool {}
function xmlwriter_write_pi(XMLWriter $writer, string $target, string $content): bool {}
function xmlwriter_start_cdata(XMLWriter $writer): bool {}
function xmlwriter_end_cdata(XMLWriter $writer): bool {}
function xmlwriter_write_cdata(XMLWriter $writer, string $content): bool {}
function xmlwriter_text(XMLWriter $writer, string $content): bool {}
function xmlwriter_write_raw(XMLWriter $writer, string $content): bool {}
function xmlwriter_start_document(XMLWriter $writer, ?string $version = '1.0', ?string $encoding = NULL, ?string $standalone = NULL): bool {}
function xmlwriter_end_document(XMLWriter $writer): bool {}
function xmlwriter_write_comment(XMLWriter $writer, string $content): bool {}
function xmlwriter_start_dtd(XMLWriter $writer, string $qualifiedName, ?string $publicId = NULL, ?string $systemId = NULL): bool {}
function xmlwriter_end_dtd(XMLWriter $writer): bool {}
function xmlwriter_write_dtd(XMLWriter $writer, string $name, ?string $publicId = NULL, ?string $systemId = NULL, ?string $content = NULL): bool {}
function xmlwriter_start_dtd_element(XMLWriter $writer, string $qualifiedName): bool {}
function xmlwriter_end_dtd_element(XMLWriter $writer): bool {}
function xmlwriter_write_dtd_element(XMLWriter $writer, string $name, string $content): bool {}
function xmlwriter_start_dtd_attlist(XMLWriter $writer, string $name): bool {}
function xmlwriter_end_dtd_attlist(XMLWriter $writer): bool {}
function xmlwriter_write_dtd_attlist(XMLWriter $writer, string $name, string $content): bool {}
function xmlwriter_start_dtd_entity(XMLWriter $writer, string $name, bool $isParam): bool {}
function xmlwriter_end_dtd_entity(XMLWriter $writer): bool {}
function xmlwriter_write_dtd_entity(XMLWriter $writer, string $name, string $content, bool $isParam = false, ?string $publicId = NULL, ?string $systemId = NULL, ?string $notationData = NULL): bool {}
function xmlwriter_output_memory(XMLWriter $writer, bool $flush = true): string {}
function xmlwriter_flush(XMLWriter $writer, bool $empty = true): string|int {}
function zip_open(string $filename) {}
function zip_close($zip): void {}
function zip_read($zip) {}
function zip_entry_open($zip_dp, $zip_entry, string $mode = 'rb'): bool {}
function zip_entry_close($zip_entry): bool {}
function zip_entry_read($zip_entry, int $len = 1024): string|false {}
function zip_entry_name($zip_entry): string|false {}
function zip_entry_compressedsize($zip_entry): int|false {}
function zip_entry_filesize($zip_entry): int|false {}
function zip_entry_compressionmethod($zip_entry): string|false {}
function opcache_reset(): bool {}
function opcache_get_status(bool $include_scripts = true): array|false {}
function opcache_compile_file(string $filename): bool {}
function opcache_invalidate(string $filename, bool $force = false): bool {}
function opcache_get_configuration(): array|false {}
function opcache_is_script_cached(string $filename): bool {}
function xdebug_break(): bool {}
function xdebug_call_class(int $depth = 2) {}
function xdebug_call_file(int $depth = 2) {}
function xdebug_call_function(int $depth = 2) {}
function xdebug_call_line(int $depth = 2) {}
function xdebug_code_coverage_started(): bool {}
function xdebug_connect_to_client(): bool {}
function xdebug_debug_zval(string $varname) {}
function xdebug_debug_zval_stdout(string $varname) {}
function xdebug_dump_superglobals() {}
function xdebug_get_code_coverage(): array {}
function xdebug_get_collected_errors(bool $emptyList = false) {}
function xdebug_get_function_count(): int {}
function xdebug_get_function_stack(): array {}
function xdebug_get_gc_run_count(): int {}
function xdebug_get_gc_total_collected_roots(): int {}
function xdebug_get_gcstats_filename() {}
function xdebug_get_headers(): array {}
function xdebug_get_monitored_functions(): array {}
function xdebug_get_profiler_filename() {}
function xdebug_get_stack_depth(): int {}
function xdebug_get_tracefile_name() {}
function xdebug_info(string $category = NULL) {}
function xdebug_is_debugger_active(): bool {}
function xdebug_memory_usage(): int {}
function xdebug_notify(mixed $data): bool {}
function xdebug_peak_memory_usage(): int {}
function xdebug_print_function_stack(string $message = 'user triggered', int $options = 0) {}
function xdebug_set_filter(int $group, int $listType, array $configuration) {}
function xdebug_start_code_coverage(int $options = 0) {}
function xdebug_start_error_collection() {}
function xdebug_start_function_monitor(array $listOfFunctionsToMonitor) {}
function xdebug_start_gcstats(?string $gcstatsFile = NULL) {}
function xdebug_start_trace(?string $traceFile = NULL, int $options = 0): ?string {}
function xdebug_stop_code_coverage(bool $cleanUp = true) {}
function xdebug_stop_error_collection() {}
function xdebug_stop_function_monitor() {}
function xdebug_stop_gcstats() {}
function xdebug_stop_trace() {}
function xdebug_time_index(): float {}
function xdebug_var_dump(mixed $variable) {}
function dl(string $extension_filename): bool {}
function cli_set_process_title(string $title): bool {}
function cli_get_process_title(): ?string {}
final class InternalIterator {
private function __construct() {}
public function current(): mixed {}
public function key(): mixed {}
public function next(): void {}
public function valid(): bool {}
public function rewind(): void {}
}
class Exception {
private function __clone(): void {}
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ErrorException {
public function __construct(string $message = '', int $code = 0, int $severity = 1, ?string $filename = NULL, ?int $line = NULL, ?Throwable $previous = NULL) {}
final public function getSeverity(): int {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class Error {
private function __clone(): void {}
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class CompileError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ParseError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class TypeError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ArgumentCountError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ValueError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ArithmeticError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class DivisionByZeroError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class UnhandledMatchError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
final class Closure {
private function __construct() {}
public static function bind(Closure $closure, ?object $newThis, object|string|null $newScope = 'static'): ?Closure {}
public function bindTo(?object $newThis, object|string|null $newScope = 'static'): ?Closure {}
public function call(object $newThis, mixed $args): mixed {}
public static function fromCallable(callable $callback): Closure {}
public function __invoke() {}
}
final class Generator {
public function rewind(): void {}
public function valid(): bool {}
public function current(): mixed {}
public function key(): mixed {}
public function next(): void {}
public function send(mixed $value): mixed {}
public function throw(Throwable $exception): mixed {}
public function getReturn(): mixed {}
}
class ClosedGeneratorException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
final class WeakReference {
public function __construct() {}
public static function create(object $object): WeakReference {}
public function get(): ?object {}
}
final class WeakMap {
public function offsetGet($object): mixed {}
public function offsetSet($object, mixed $value): void {}
public function offsetExists($object): bool {}
public function offsetUnset($object): void {}
public function count(): int {}
public function getIterator(): Iterator {}
}
final class Attribute {
public function __construct(int $flags = 63) {}
}
final class ReturnTypeWillChange {
public function __construct() {}
}
final class AllowDynamicProperties {
public function __construct() {}
}
final class SensitiveParameter {
public function __construct() {}
}
final class SensitiveParameterValue {
public function __construct(mixed $value) {}
public function getValue(): mixed {}
public function __debugInfo(): array {}
}
final class Fiber {
public function __construct(callable $callback) {}
public function start(mixed $args): mixed {}
public function resume(mixed $value = NULL): mixed {}
public function throw(Throwable $exception): mixed {}
public function isStarted(): bool {}
public function isSuspended(): bool {}
public function isRunning(): bool {}
public function isTerminated(): bool {}
public function getReturn(): mixed {}
public static function getCurrent(): ?Fiber {}
public static function suspend(mixed $value = NULL): mixed {}
}
final class FiberError {
public function __construct() {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class stdClass {
}
class DateTime {
public function __construct(string $datetime = 'now', ?DateTimeZone $timezone = NULL) {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function __wakeup(): ?void {}
public static function __set_state(array $array): ?DateTime {}
public static function createFromImmutable(DateTimeImmutable $object): ?static {}
public static function createFromInterface(DateTimeInterface $object): DateTime {}
public static function createFromFormat(string $format, string $datetime, ?DateTimeZone $timezone = NULL): ?DateTime|false {}
public static function getLastErrors(): ?array|false {}
public function format(string $format): ?string {}
public function modify(string $modifier): ?DateTime|false {}
public function add(DateInterval $interval): ?DateTime {}
public function sub(DateInterval $interval): ?DateTime {}
public function getTimezone(): ?DateTimeZone|false {}
public function setTimezone(DateTimeZone $timezone): ?DateTime {}
public function getOffset(): ?int {}
public function setTime(int $hour, int $minute, int $second = 0, int $microsecond = 0): ?DateTime {}
public function setDate(int $year, int $month, int $day): ?DateTime {}
public function setISODate(int $year, int $week, int $dayOfWeek = 1): ?DateTime {}
public function setTimestamp(int $timestamp): ?DateTime {}
public function getTimestamp(): ?int {}
public function diff(DateTimeInterface $targetObject, bool $absolute = false): ?DateInterval {}
}
class DateTimeImmutable {
public function __construct(string $datetime = 'now', ?DateTimeZone $timezone = NULL) {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function __wakeup(): ?void {}
public static function __set_state(array $array): ?DateTimeImmutable {}
public static function createFromFormat(string $format, string $datetime, ?DateTimeZone $timezone = NULL): ?DateTimeImmutable|false {}
public static function getLastErrors(): ?array|false {}
public function format(string $format): ?string {}
public function getTimezone(): ?DateTimeZone|false {}
public function getOffset(): ?int {}
public function getTimestamp(): ?int {}
public function diff(DateTimeInterface $targetObject, bool $absolute = false): ?DateInterval {}
public function modify(string $modifier): ?DateTimeImmutable|false {}
public function add(DateInterval $interval): ?DateTimeImmutable {}
public function sub(DateInterval $interval): ?DateTimeImmutable {}
public function setTimezone(DateTimeZone $timezone): ?DateTimeImmutable {}
public function setTime(int $hour, int $minute, int $second = 0, int $microsecond = 0): ?DateTimeImmutable {}
public function setDate(int $year, int $month, int $day): ?DateTimeImmutable {}
public function setISODate(int $year, int $week, int $dayOfWeek = 1): ?DateTimeImmutable {}
public function setTimestamp(int $timestamp): ?DateTimeImmutable {}
public static function createFromMutable(DateTime $object): ?static {}
public static function createFromInterface(DateTimeInterface $object): DateTimeImmutable {}
}
class DateTimeZone {
public function __construct(string $timezone) {}
public function getName(): ?string {}
public function getOffset(DateTimeInterface $datetime): ?int {}
public function getTransitions(int $timestampBegin = -9223372036854775807-1, int $timestampEnd = 9223372036854775807): ?array|false {}
public function getLocation(): ?array|false {}
public static function listAbbreviations(): ?array {}
public static function listIdentifiers(int $timezoneGroup = 2047, ?string $countryCode = NULL): ?array {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function __wakeup(): ?void {}
public static function __set_state(array $array): ?DateTimeZone {}
}
class DateInterval {
public function __construct(string $duration) {}
public static function createFromDateString(string $datetime): ?DateInterval|false {}
public function format(string $format): ?string {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function __wakeup(): ?void {}
public static function __set_state(array $array): ?DateInterval {}
}
class DatePeriod {
public function __construct($start, $interval, $end, $options) {}
public function getStartDate(): ?DateTimeInterface {}
public function getEndDate(): ??DateTimeInterface {}
public function getDateInterval(): ?DateInterval {}
public function getRecurrences(): ??int {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function __wakeup(): ?void {}
public static function __set_state(array $array): ?DatePeriod {}
public function getIterator(): Iterator {}
}
class LibXMLError {
}
final class OpenSSLCertificate {
}
final class OpenSSLCertificateSigningRequest {
}
final class OpenSSLAsymmetricKey {
}
final class InflateContext {
}
final class DeflateContext {
}
final class HashContext {
private function __construct() {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
}
class JsonException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
namespace Random {
class RandomError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
}
namespace Random {
class BrokenRandomEngineError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
}
namespace Random {
class RandomException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
}
namespace Random\Engine {
final class Mt19937 {
public function __construct(?int $seed = NULL, int $mode = 0) {}
public function generate(): string {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function __debugInfo(): array {}
}
}
namespace Random\Engine {
final class PcgOneseq128XslRr64 {
public function __construct(string|int|null $seed = NULL) {}
public function generate(): string {}
public function jump(int $advance): void {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function __debugInfo(): array {}
}
}
namespace Random\Engine {
final class Xoshiro256StarStar {
public function __construct(string|int|null $seed = NULL) {}
public function generate(): string {}
public function jump(): void {}
public function jumpLong(): void {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function __debugInfo(): array {}
}
}
namespace Random\Engine {
final class Secure {
public function generate(): string {}
}
}
namespace Random {
final class Randomizer {
public function __construct(?Random\Engine $engine = NULL) {}
public function nextInt(): int {}
public function getInt(int $min, int $max): int {}
public function getBytes(int $length): string {}
public function shuffleArray(array $array): array {}
public function shuffleBytes(string $bytes): string {}
public function pickArrayKeys(array $array, int $num): array {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
}
}
class ReflectionException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class Reflection {
public static function getModifierNames(int $modifiers): ?array {}
}
abstract class ReflectionFunctionAbstract {
private function __clone(): void {}
public function inNamespace(): ?bool {}
public function isClosure(): ?bool {}
public function isDeprecated(): ?bool {}
public function isInternal(): ?bool {}
public function isUserDefined(): ?bool {}
public function isGenerator(): ?bool {}
public function isVariadic(): ?bool {}
public function isStatic(): ?bool {}
public function getClosureThis(): ??object {}
public function getClosureScopeClass(): ??ReflectionClass {}
public function getClosureCalledClass(): ??ReflectionClass {}
public function getClosureUsedVariables(): array {}
public function getDocComment(): ?string|false {}
public function getEndLine(): ?int|false {}
public function getExtension(): ??ReflectionExtension {}
public function getExtensionName(): ?string|false {}
public function getFileName(): ?string|false {}
public function getName(): ?string {}
public function getNamespaceName(): ?string {}
public function getNumberOfParameters(): ?int {}
public function getNumberOfRequiredParameters(): ?int {}
public function getParameters(): ?array {}
public function getShortName(): ?string {}
public function getStartLine(): ?int|false {}
public function getStaticVariables(): ?array {}
public function returnsReference(): ?bool {}
public function hasReturnType(): ?bool {}
public function getReturnType(): ??ReflectionType {}
public function hasTentativeReturnType(): bool {}
public function getTentativeReturnType(): ?ReflectionType {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
public function __toString(): string {}
}
class ReflectionFunction {
public function __construct(Closure|string $function) {}
public function __toString(): string {}
public function isAnonymous(): bool {}
public function isDisabled(): ?bool {}
public function invoke(mixed $args): ?mixed {}
public function invokeArgs(array $args): ?mixed {}
public function getClosure(): ?Closure {}
public function inNamespace(): ?bool {}
public function isClosure(): ?bool {}
public function isDeprecated(): ?bool {}
public function isInternal(): ?bool {}
public function isUserDefined(): ?bool {}
public function isGenerator(): ?bool {}
public function isVariadic(): ?bool {}
public function isStatic(): ?bool {}
public function getClosureThis(): ??object {}
public function getClosureScopeClass(): ??ReflectionClass {}
public function getClosureCalledClass(): ??ReflectionClass {}
public function getClosureUsedVariables(): array {}
public function getDocComment(): ?string|false {}
public function getEndLine(): ?int|false {}
public function getExtension(): ??ReflectionExtension {}
public function getExtensionName(): ?string|false {}
public function getFileName(): ?string|false {}
public function getName(): ?string {}
public function getNamespaceName(): ?string {}
public function getNumberOfParameters(): ?int {}
public function getNumberOfRequiredParameters(): ?int {}
public function getParameters(): ?array {}
public function getShortName(): ?string {}
public function getStartLine(): ?int|false {}
public function getStaticVariables(): ?array {}
public function returnsReference(): ?bool {}
public function hasReturnType(): ?bool {}
public function getReturnType(): ??ReflectionType {}
public function hasTentativeReturnType(): bool {}
public function getTentativeReturnType(): ?ReflectionType {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
}
final class ReflectionGenerator {
public function __construct(Generator $generator) {}
public function getExecutingLine(): ?int {}
public function getExecutingFile(): ?string {}
public function getTrace(int $options = 1): ?array {}
public function getFunction(): ?ReflectionFunctionAbstract {}
public function getThis(): ??object {}
public function getExecutingGenerator(): ?Generator {}
}
class ReflectionParameter {
private function __clone(): void {}
public function __construct($function, string|int $param) {}
public function __toString(): string {}
public function getName(): ?string {}
public function isPassedByReference(): ?bool {}
public function canBePassedByValue(): ?bool {}
public function getDeclaringFunction(): ?ReflectionFunctionAbstract {}
public function getDeclaringClass(): ??ReflectionClass {}
public function getClass(): ??ReflectionClass {}
public function hasType(): ?bool {}
public function getType(): ??ReflectionType {}
public function isArray(): ?bool {}
public function isCallable(): ?bool {}
public function allowsNull(): ?bool {}
public function getPosition(): ?int {}
public function isOptional(): ?bool {}
public function isDefaultValueAvailable(): ?bool {}
public function getDefaultValue(): ?mixed {}
public function isDefaultValueConstant(): ?bool {}
public function getDefaultValueConstantName(): ??string {}
public function isVariadic(): ?bool {}
public function isPromoted(): bool {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
}
abstract class ReflectionType {
private function __clone(): void {}
public function allowsNull(): ?bool {}
public function __toString(): string {}
}
class ReflectionNamedType {
public function getName(): ?string {}
public function isBuiltin(): ?bool {}
public function allowsNull(): ?bool {}
public function __toString(): string {}
}
class ReflectionUnionType {
public function getTypes(): array {}
public function allowsNull(): ?bool {}
public function __toString(): string {}
}
class ReflectionIntersectionType {
public function getTypes(): array {}
public function allowsNull(): ?bool {}
public function __toString(): string {}
}
class ReflectionMethod {
public function __construct(object|string $objectOrMethod, ?string $method = NULL) {}
public function __toString(): string {}
public function isPublic(): ?bool {}
public function isPrivate(): ?bool {}
public function isProtected(): ?bool {}
public function isAbstract(): ?bool {}
public function isFinal(): ?bool {}
public function isConstructor(): ?bool {}
public function isDestructor(): ?bool {}
public function getClosure(?object $object = NULL): ?Closure {}
public function getModifiers(): ?int {}
public function invoke(?object $object, mixed $args): ?mixed {}
public function invokeArgs(?object $object, array $args): ?mixed {}
public function getDeclaringClass(): ?ReflectionClass {}
public function getPrototype(): ?ReflectionMethod {}
public function hasPrototype(): bool {}
public function setAccessible(bool $accessible): ?void {}
public function inNamespace(): ?bool {}
public function isClosure(): ?bool {}
public function isDeprecated(): ?bool {}
public function isInternal(): ?bool {}
public function isUserDefined(): ?bool {}
public function isGenerator(): ?bool {}
public function isVariadic(): ?bool {}
public function isStatic(): ?bool {}
public function getClosureThis(): ??object {}
public function getClosureScopeClass(): ??ReflectionClass {}
public function getClosureCalledClass(): ??ReflectionClass {}
public function getClosureUsedVariables(): array {}
public function getDocComment(): ?string|false {}
public function getEndLine(): ?int|false {}
public function getExtension(): ??ReflectionExtension {}
public function getExtensionName(): ?string|false {}
public function getFileName(): ?string|false {}
public function getName(): ?string {}
public function getNamespaceName(): ?string {}
public function getNumberOfParameters(): ?int {}
public function getNumberOfRequiredParameters(): ?int {}
public function getParameters(): ?array {}
public function getShortName(): ?string {}
public function getStartLine(): ?int|false {}
public function getStaticVariables(): ?array {}
public function returnsReference(): ?bool {}
public function hasReturnType(): ?bool {}
public function getReturnType(): ??ReflectionType {}
public function hasTentativeReturnType(): bool {}
public function getTentativeReturnType(): ?ReflectionType {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
}
class ReflectionClass {
private function __clone(): void {}
public function __construct(object|string $objectOrClass) {}
public function __toString(): string {}
public function getName(): ?string {}
public function isInternal(): ?bool {}
public function isUserDefined(): ?bool {}
public function isAnonymous(): ?bool {}
public function isInstantiable(): ?bool {}
public function isCloneable(): ?bool {}
public function getFileName(): ?string|false {}
public function getStartLine(): ?int|false {}
public function getEndLine(): ?int|false {}
public function getDocComment(): ?string|false {}
public function getConstructor(): ??ReflectionMethod {}
public function hasMethod(string $name): ?bool {}
public function getMethod(string $name): ?ReflectionMethod {}
public function getMethods(?int $filter = NULL): ?array {}
public function hasProperty(string $name): ?bool {}
public function getProperty(string $name): ?ReflectionProperty {}
public function getProperties(?int $filter = NULL): ?array {}
public function hasConstant(string $name): ?bool {}
public function getConstants(?int $filter = NULL): ?array {}
public function getReflectionConstants(?int $filter = NULL): ?array {}
public function getConstant(string $name): ?mixed {}
public function getReflectionConstant(string $name): ?ReflectionClassConstant|false {}
public function getInterfaces(): ?array {}
public function getInterfaceNames(): ?array {}
public function isInterface(): ?bool {}
public function getTraits(): ?array {}
public function getTraitNames(): ?array {}
public function getTraitAliases(): ?array {}
public function isTrait(): ?bool {}
public function isEnum(): bool {}
public function isAbstract(): ?bool {}
public function isFinal(): ?bool {}
public function isReadOnly(): bool {}
public function getModifiers(): ?int {}
public function isInstance(object $object): ?bool {}
public function newInstance(mixed $args): ?object {}
public function newInstanceWithoutConstructor(): ?object {}
public function newInstanceArgs(array $args = array (
)): ??object {}
public function getParentClass(): ?ReflectionClass|false {}
public function isSubclassOf(ReflectionClass|string $class): ?bool {}
public function getStaticProperties(): ??array {}
public function getStaticPropertyValue(string $name, mixed $default): ?mixed {}
public function setStaticPropertyValue(string $name, mixed $value): ?void {}
public function getDefaultProperties(): ?array {}
public function isIterable(): ?bool {}
public function isIterateable(): ?bool {}
public function implementsInterface(ReflectionClass|string $interface): ?bool {}
public function getExtension(): ??ReflectionExtension {}
public function getExtensionName(): ?string|false {}
public function inNamespace(): ?bool {}
public function getNamespaceName(): ?string {}
public function getShortName(): ?string {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
}
class ReflectionObject {
public function __construct(object $object) {}
public function __toString(): string {}
public function getName(): ?string {}
public function isInternal(): ?bool {}
public function isUserDefined(): ?bool {}
public function isAnonymous(): ?bool {}
public function isInstantiable(): ?bool {}
public function isCloneable(): ?bool {}
public function getFileName(): ?string|false {}
public function getStartLine(): ?int|false {}
public function getEndLine(): ?int|false {}
public function getDocComment(): ?string|false {}
public function getConstructor(): ??ReflectionMethod {}
public function hasMethod(string $name): ?bool {}
public function getMethod(string $name): ?ReflectionMethod {}
public function getMethods(?int $filter = NULL): ?array {}
public function hasProperty(string $name): ?bool {}
public function getProperty(string $name): ?ReflectionProperty {}
public function getProperties(?int $filter = NULL): ?array {}
public function hasConstant(string $name): ?bool {}
public function getConstants(?int $filter = NULL): ?array {}
public function getReflectionConstants(?int $filter = NULL): ?array {}
public function getConstant(string $name): ?mixed {}
public function getReflectionConstant(string $name): ?ReflectionClassConstant|false {}
public function getInterfaces(): ?array {}
public function getInterfaceNames(): ?array {}
public function isInterface(): ?bool {}
public function getTraits(): ?array {}
public function getTraitNames(): ?array {}
public function getTraitAliases(): ?array {}
public function isTrait(): ?bool {}
public function isEnum(): bool {}
public function isAbstract(): ?bool {}
public function isFinal(): ?bool {}
public function isReadOnly(): bool {}
public function getModifiers(): ?int {}
public function isInstance(object $object): ?bool {}
public function newInstance(mixed $args): ?object {}
public function newInstanceWithoutConstructor(): ?object {}
public function newInstanceArgs(array $args = array (
)): ??object {}
public function getParentClass(): ?ReflectionClass|false {}
public function isSubclassOf(ReflectionClass|string $class): ?bool {}
public function getStaticProperties(): ??array {}
public function getStaticPropertyValue(string $name, mixed $default): ?mixed {}
public function setStaticPropertyValue(string $name, mixed $value): ?void {}
public function getDefaultProperties(): ?array {}
public function isIterable(): ?bool {}
public function isIterateable(): ?bool {}
public function implementsInterface(ReflectionClass|string $interface): ?bool {}
public function getExtension(): ??ReflectionExtension {}
public function getExtensionName(): ?string|false {}
public function inNamespace(): ?bool {}
public function getNamespaceName(): ?string {}
public function getShortName(): ?string {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
}
class ReflectionProperty {
private function __clone(): void {}
public function __construct(object|string $class, string $property) {}
public function __toString(): string {}
public function getName(): ?string {}
public function getValue(?object $object = NULL): ?mixed {}
public function setValue(mixed $objectOrValue, mixed $value): ?void {}
public function isInitialized(?object $object = NULL): ?bool {}
public function isPublic(): ?bool {}
public function isPrivate(): ?bool {}
public function isProtected(): ?bool {}
public function isStatic(): ?bool {}
public function isReadOnly(): bool {}
public function isDefault(): ?bool {}
public function isPromoted(): bool {}
public function getModifiers(): ?int {}
public function getDeclaringClass(): ?ReflectionClass {}
public function getDocComment(): ?string|false {}
public function setAccessible(bool $accessible): ?void {}
public function getType(): ??ReflectionType {}
public function hasType(): ?bool {}
public function hasDefaultValue(): bool {}
public function getDefaultValue(): ?mixed {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
}
class ReflectionClassConstant {
private function __clone(): void {}
public function __construct(object|string $class, string $constant) {}
public function __toString(): string {}
public function getName(): ?string {}
public function getValue(): ?mixed {}
public function isPublic(): ?bool {}
public function isPrivate(): ?bool {}
public function isProtected(): ?bool {}
public function isFinal(): bool {}
public function getModifiers(): ?int {}
public function getDeclaringClass(): ?ReflectionClass {}
public function getDocComment(): ?string|false {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
public function isEnumCase(): bool {}
}
class ReflectionExtension {
private function __clone(): void {}
public function __construct(string $name) {}
public function __toString(): string {}
public function getName(): ?string {}
public function getVersion(): ??string {}
public function getFunctions(): ?array {}
public function getConstants(): ?array {}
public function getINIEntries(): ?array {}
public function getClasses(): ?array {}
public function getClassNames(): ?array {}
public function getDependencies(): ?array {}
public function info(): ?void {}
public function isPersistent(): ?bool {}
public function isTemporary(): ?bool {}
}
class ReflectionZendExtension {
private function __clone(): void {}
public function __construct(string $name) {}
public function __toString(): string {}
public function getName(): ?string {}
public function getVersion(): ?string {}
public function getAuthor(): ?string {}
public function getURL(): ?string {}
public function getCopyright(): ?string {}
}
final class ReflectionReference {
public static function fromArrayElement(array $array, string|int $key): ?ReflectionReference {}
public function getId(): string {}
private function __clone(): void {}
private function __construct() {}
}
class ReflectionAttribute {
public function getName(): string {}
public function getTarget(): int {}
public function isRepeated(): bool {}
public function getArguments(): array {}
public function newInstance(): object {}
public function __toString(): string {}
private function __clone(): void {}
private function __construct() {}
}
class ReflectionEnum {
public function __construct(object|string $objectOrClass) {}
public function hasCase(string $name): bool {}
public function getCase(string $name): ReflectionEnumUnitCase {}
public function getCases(): array {}
public function isBacked(): bool {}
public function getBackingType(): ?ReflectionNamedType {}
public function __toString(): string {}
public function getName(): ?string {}
public function isInternal(): ?bool {}
public function isUserDefined(): ?bool {}
public function isAnonymous(): ?bool {}
public function isInstantiable(): ?bool {}
public function isCloneable(): ?bool {}
public function getFileName(): ?string|false {}
public function getStartLine(): ?int|false {}
public function getEndLine(): ?int|false {}
public function getDocComment(): ?string|false {}
public function getConstructor(): ??ReflectionMethod {}
public function hasMethod(string $name): ?bool {}
public function getMethod(string $name): ?ReflectionMethod {}
public function getMethods(?int $filter = NULL): ?array {}
public function hasProperty(string $name): ?bool {}
public function getProperty(string $name): ?ReflectionProperty {}
public function getProperties(?int $filter = NULL): ?array {}
public function hasConstant(string $name): ?bool {}
public function getConstants(?int $filter = NULL): ?array {}
public function getReflectionConstants(?int $filter = NULL): ?array {}
public function getConstant(string $name): ?mixed {}
public function getReflectionConstant(string $name): ?ReflectionClassConstant|false {}
public function getInterfaces(): ?array {}
public function getInterfaceNames(): ?array {}
public function isInterface(): ?bool {}
public function getTraits(): ?array {}
public function getTraitNames(): ?array {}
public function getTraitAliases(): ?array {}
public function isTrait(): ?bool {}
public function isEnum(): bool {}
public function isAbstract(): ?bool {}
public function isFinal(): ?bool {}
public function isReadOnly(): bool {}
public function getModifiers(): ?int {}
public function isInstance(object $object): ?bool {}
public function newInstance(mixed $args): ?object {}
public function newInstanceWithoutConstructor(): ?object {}
public function newInstanceArgs(array $args = array (
)): ??object {}
public function getParentClass(): ?ReflectionClass|false {}
public function isSubclassOf(ReflectionClass|string $class): ?bool {}
public function getStaticProperties(): ??array {}
public function getStaticPropertyValue(string $name, mixed $default): ?mixed {}
public function setStaticPropertyValue(string $name, mixed $value): ?void {}
public function getDefaultProperties(): ?array {}
public function isIterable(): ?bool {}
public function isIterateable(): ?bool {}
public function implementsInterface(ReflectionClass|string $interface): ?bool {}
public function getExtension(): ??ReflectionExtension {}
public function getExtensionName(): ?string|false {}
public function inNamespace(): ?bool {}
public function getNamespaceName(): ?string {}
public function getShortName(): ?string {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
}
class ReflectionEnumUnitCase {
public function __construct(object|string $class, string $constant) {}
public function getEnum(): ReflectionEnum {}
public function getValue(): UnitEnum {}
public function __toString(): string {}
public function getName(): ?string {}
public function isPublic(): ?bool {}
public function isPrivate(): ?bool {}
public function isProtected(): ?bool {}
public function isFinal(): bool {}
public function getModifiers(): ?int {}
public function getDeclaringClass(): ?ReflectionClass {}
public function getDocComment(): ?string|false {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
public function isEnumCase(): bool {}
}
class ReflectionEnumBackedCase {
public function __construct(object|string $class, string $constant) {}
public function getBackingValue(): string|int {}
public function getEnum(): ReflectionEnum {}
public function getValue(): UnitEnum {}
public function __toString(): string {}
public function getName(): ?string {}
public function isPublic(): ?bool {}
public function isPrivate(): ?bool {}
public function isProtected(): ?bool {}
public function isFinal(): bool {}
public function getModifiers(): ?int {}
public function getDeclaringClass(): ?ReflectionClass {}
public function getDocComment(): ?string|false {}
public function getAttributes(?string $name = NULL, int $flags = 0): array {}
public function isEnumCase(): bool {}
}
final class ReflectionFiber {
public function __construct(Fiber $fiber) {}
public function getFiber(): Fiber {}
public function getExecutingFile(): ?string {}
public function getExecutingLine(): ?int {}
public function getCallable(): callable {}
public function getTrace(int $options = 1): array {}
}
class LogicException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class BadFunctionCallException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class BadMethodCallException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class DomainException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class InvalidArgumentException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class LengthException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class OutOfRangeException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class RuntimeException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class OutOfBoundsException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class OverflowException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class RangeException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class UnderflowException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class UnexpectedValueException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class RecursiveIteratorIterator {
public function __construct(Traversable $iterator, int $mode = 0, int $flags = 0) {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
public function next(): ?void {}
public function getDepth(): ?int {}
public function getSubIterator(?int $level = NULL): ??RecursiveIterator {}
public function getInnerIterator(): ?RecursiveIterator {}
public function beginIteration(): ?void {}
public function endIteration(): ?void {}
public function callHasChildren(): ?bool {}
public function callGetChildren(): ??RecursiveIterator {}
public function beginChildren(): ?void {}
public function endChildren(): ?void {}
public function nextElement(): ?void {}
public function setMaxDepth(int $maxDepth = -1): ?void {}
public function getMaxDepth(): ?int|false {}
}
class IteratorIterator {
public function __construct(Traversable $iterator, ?string $class = NULL) {}
public function getInnerIterator(): ??Iterator {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
public function next(): ?void {}
}
abstract class FilterIterator {
public function accept(): ?bool {}
public function __construct(Iterator $iterator) {}
public function rewind(): ?void {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
abstract class RecursiveFilterIterator {
public function __construct(RecursiveIterator $iterator) {}
public function hasChildren(): ?bool {}
public function getChildren(): ??RecursiveFilterIterator {}
public function accept(): ?bool {}
public function rewind(): ?void {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class CallbackFilterIterator {
public function __construct(Iterator $iterator, callable $callback) {}
public function accept(): ?bool {}
public function rewind(): ?void {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class RecursiveCallbackFilterIterator {
public function __construct(RecursiveIterator $iterator, callable $callback) {}
public function hasChildren(): ?bool {}
public function getChildren(): ?RecursiveCallbackFilterIterator {}
public function accept(): ?bool {}
public function rewind(): ?void {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class ParentIterator {
public function __construct(RecursiveIterator $iterator) {}
public function accept(): ?bool {}
public function hasChildren(): ?bool {}
public function getChildren(): ??RecursiveFilterIterator {}
public function rewind(): ?void {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class LimitIterator {
public function __construct(Iterator $iterator, int $offset = 0, int $limit = -1) {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function next(): ?void {}
public function seek(int $offset): ?int {}
public function getPosition(): ?int {}
public function getInnerIterator(): ??Iterator {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class CachingIterator {
public function __construct(Iterator $iterator, int $flags = 1) {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function next(): ?void {}
public function hasNext(): ?bool {}
public function __toString(): string {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function offsetGet($key): ?mixed {}
public function offsetSet($key, mixed $value): ?void {}
public function offsetUnset($key): ?void {}
public function offsetExists($key): ?bool {}
public function getCache(): ?array {}
public function count(): ?int {}
public function getInnerIterator(): ??Iterator {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class RecursiveCachingIterator {
public function __construct(Iterator $iterator, int $flags = 1) {}
public function hasChildren(): ?bool {}
public function getChildren(): ??RecursiveCachingIterator {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function next(): ?void {}
public function hasNext(): ?bool {}
public function __toString(): string {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function offsetGet($key): ?mixed {}
public function offsetSet($key, mixed $value): ?void {}
public function offsetUnset($key): ?void {}
public function offsetExists($key): ?bool {}
public function getCache(): ?array {}
public function count(): ?int {}
public function getInnerIterator(): ??Iterator {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class NoRewindIterator {
public function __construct(Iterator $iterator) {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
}
class AppendIterator {
public function __construct() {}
public function append(Iterator $iterator): ?void {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function current(): ?mixed {}
public function next(): ?void {}
public function getIteratorIndex(): ??int {}
public function getArrayIterator(): ?ArrayIterator {}
public function getInnerIterator(): ??Iterator {}
public function key(): ?mixed {}
}
class InfiniteIterator {
public function __construct(Iterator $iterator) {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class RegexIterator {
public function __construct(Iterator $iterator, string $pattern, int $mode = 0, int $flags = 0, int $pregFlags = 0) {}
public function accept(): ?bool {}
public function getMode(): ?int {}
public function setMode(int $mode): ?void {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function getRegex(): ?string {}
public function getPregFlags(): ?int {}
public function setPregFlags(int $pregFlags): ?void {}
public function rewind(): ?void {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class RecursiveRegexIterator {
public function __construct(RecursiveIterator $iterator, string $pattern, int $mode = 0, int $flags = 0, int $pregFlags = 0) {}
public function accept(): ?bool {}
public function hasChildren(): ?bool {}
public function getChildren(): ?RecursiveRegexIterator {}
public function getMode(): ?int {}
public function setMode(int $mode): ?void {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function getRegex(): ?string {}
public function getPregFlags(): ?int {}
public function setPregFlags(int $pregFlags): ?void {}
public function rewind(): ?void {}
public function next(): ?void {}
public function getInnerIterator(): ??Iterator {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
}
class EmptyIterator {
public function current(): ?never {}
public function next(): ?void {}
public function key(): ?never {}
public function valid(): ?false {}
public function rewind(): ?void {}
}
class RecursiveTreeIterator {
public function __construct($iterator, int $flags = 8, int $cachingIteratorFlags = 16, int $mode = 1) {}
public function key(): ?mixed {}
public function current(): ?mixed {}
public function getPrefix(): ?string {}
public function setPostfix(string $postfix): ?void {}
public function setPrefixPart(int $part, string $value): ?void {}
public function getEntry(): ?string {}
public function getPostfix(): ?string {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function next(): ?void {}
public function getDepth(): ?int {}
public function getSubIterator(?int $level = NULL): ??RecursiveIterator {}
public function getInnerIterator(): ?RecursiveIterator {}
public function beginIteration(): ?void {}
public function endIteration(): ?void {}
public function callHasChildren(): ?bool {}
public function callGetChildren(): ??RecursiveIterator {}
public function beginChildren(): ?void {}
public function endChildren(): ?void {}
public function nextElement(): ?void {}
public function setMaxDepth(int $maxDepth = -1): ?void {}
public function getMaxDepth(): ?int|false {}
}
class ArrayObject {
public function __construct(object|array $array = array (
), int $flags = 0, string $iteratorClass = 'ArrayIterator') {}
public function offsetExists(mixed $key): ?bool {}
public function offsetGet(mixed $key): ?mixed {}
public function offsetSet(mixed $key, mixed $value): ?void {}
public function offsetUnset(mixed $key): ?void {}
public function append(mixed $value): ?void {}
public function getArrayCopy(): ?array {}
public function count(): ?int {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function asort(int $flags = 0): ?bool {}
public function ksort(int $flags = 0): ?bool {}
public function uasort(callable $callback): ?bool {}
public function uksort(callable $callback): ?bool {}
public function natsort(): ?bool {}
public function natcasesort(): ?bool {}
public function unserialize(string $data): ?void {}
public function serialize(): ?string {}
public function __serialize(): ?array {}
public function __unserialize(array $data): ?void {}
public function getIterator(): ?Iterator {}
public function exchangeArray(object|array $array): ?array {}
public function setIteratorClass(string $iteratorClass): ?void {}
public function getIteratorClass(): ?string {}
public function __debugInfo(): ?array {}
}
class ArrayIterator {
public function __construct(object|array $array = array (
), int $flags = 0) {}
public function offsetExists(mixed $key): ?bool {}
public function offsetGet(mixed $key): ?mixed {}
public function offsetSet(mixed $key, mixed $value): ?void {}
public function offsetUnset(mixed $key): ?void {}
public function append(mixed $value): ?void {}
public function getArrayCopy(): ?array {}
public function count(): ?int {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function asort(int $flags = 0): ?bool {}
public function ksort(int $flags = 0): ?bool {}
public function uasort(callable $callback): ?bool {}
public function uksort(callable $callback): ?bool {}
public function natsort(): ?bool {}
public function natcasesort(): ?bool {}
public function unserialize(string $data): ?void {}
public function serialize(): ?string {}
public function __serialize(): ?array {}
public function __unserialize(array $data): ?void {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?string|int|null {}
public function next(): ?void {}
public function valid(): ?bool {}
public function seek(int $offset): ?void {}
public function __debugInfo(): ?array {}
}
class RecursiveArrayIterator {
public function hasChildren(): ?bool {}
public function getChildren(): ??RecursiveArrayIterator {}
public function __construct(object|array $array = array (
), int $flags = 0) {}
public function offsetExists(mixed $key): ?bool {}
public function offsetGet(mixed $key): ?mixed {}
public function offsetSet(mixed $key, mixed $value): ?void {}
public function offsetUnset(mixed $key): ?void {}
public function append(mixed $value): ?void {}
public function getArrayCopy(): ?array {}
public function count(): ?int {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function asort(int $flags = 0): ?bool {}
public function ksort(int $flags = 0): ?bool {}
public function uasort(callable $callback): ?bool {}
public function uksort(callable $callback): ?bool {}
public function natsort(): ?bool {}
public function natcasesort(): ?bool {}
public function unserialize(string $data): ?void {}
public function serialize(): ?string {}
public function __serialize(): ?array {}
public function __unserialize(array $data): ?void {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?string|int|null {}
public function next(): ?void {}
public function valid(): ?bool {}
public function seek(int $offset): ?void {}
public function __debugInfo(): ?array {}
}
class SplFileInfo {
public function __construct(string $filename) {}
public function getPath(): ?string {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isWritable(): ?bool {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __toString(): string {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class DirectoryIterator {
public function __construct(string $directory) {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function isDot(): ?bool {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function key(): ?mixed {}
public function current(): ?mixed {}
public function next(): ?void {}
public function seek(int $offset): ?void {}
public function __toString(): string {}
public function getPath(): ?string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isWritable(): ?bool {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class FilesystemIterator {
public function __construct(string $directory, int $flags = 4096) {}
public function rewind(): ?void {}
public function key(): ?string {}
public function current(): ?SplFileInfo|FilesystemIterator|string {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function isDot(): ?bool {}
public function valid(): ?bool {}
public function next(): ?void {}
public function seek(int $offset): ?void {}
public function __toString(): string {}
public function getPath(): ?string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isWritable(): ?bool {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class RecursiveDirectoryIterator {
public function __construct(string $directory, int $flags = 0) {}
public function hasChildren(bool $allowLinks = false): ?bool {}
public function getChildren(): ?RecursiveDirectoryIterator {}
public function getSubPath(): ?string {}
public function getSubPathname(): ?string {}
public function rewind(): ?void {}
public function key(): ?string {}
public function current(): ?SplFileInfo|FilesystemIterator|string {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function isDot(): ?bool {}
public function valid(): ?bool {}
public function next(): ?void {}
public function seek(int $offset): ?void {}
public function __toString(): string {}
public function getPath(): ?string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isWritable(): ?bool {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class GlobIterator {
public function __construct(string $pattern, int $flags = 0) {}
public function count(): ?int {}
public function rewind(): ?void {}
public function key(): ?string {}
public function current(): ?SplFileInfo|FilesystemIterator|string {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function isDot(): ?bool {}
public function valid(): ?bool {}
public function next(): ?void {}
public function seek(int $offset): ?void {}
public function __toString(): string {}
public function getPath(): ?string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isWritable(): ?bool {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class SplFileObject {
public function __construct(string $filename, string $mode = 'r', bool $useIncludePath = false, $context = NULL) {}
public function rewind(): ?void {}
public function eof(): ?bool {}
public function valid(): ?bool {}
public function fgets(): ?string {}
public function fread(int $length): ?string|false {}
public function fgetcsv(string $separator = ',', string $enclosure = '"', string $escape = '\\'): ?array|false {}
public function fputcsv(array $fields, string $separator = ',', string $enclosure = '"', string $escape = '\\', string $eol = '
'): ?int|false {}
public function setCsvControl(string $separator = ',', string $enclosure = '"', string $escape = '\\'): ?void {}
public function getCsvControl(): ?array {}
public function flock(int $operation, $wouldBlock = NULL): ?bool {}
public function fflush(): ?bool {}
public function ftell(): ?int|false {}
public function fseek(int $offset, int $whence = 0): ?int {}
public function fgetc(): ?string|false {}
public function fpassthru(): ?int {}
public function fscanf(string $format, mixed $vars): ?array|int|null {}
public function fwrite(string $data, int $length = 0): ?int|false {}
public function fstat(): ?array {}
public function ftruncate(int $size): ?bool {}
public function current(): ?array|string|false {}
public function key(): ?int {}
public function next(): ?void {}
public function setFlags(int $flags): ?void {}
public function getFlags(): ?int {}
public function setMaxLineLen(int $maxLength): ?void {}
public function getMaxLineLen(): ?int {}
public function hasChildren(): ?false {}
public function getChildren(): ?null {}
public function seek(int $line): ?void {}
public function getCurrentLine(): ?string {}
public function __toString(): string {}
public function getPath(): ?string {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isWritable(): ?bool {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class SplTempFileObject {
public function __construct(int $maxMemory = 2097152) {}
public function rewind(): ?void {}
public function eof(): ?bool {}
public function valid(): ?bool {}
public function fgets(): ?string {}
public function fread(int $length): ?string|false {}
public function fgetcsv(string $separator = ',', string $enclosure = '"', string $escape = '\\'): ?array|false {}
public function fputcsv(array $fields, string $separator = ',', string $enclosure = '"', string $escape = '\\', string $eol = '
'): ?int|false {}
public function setCsvControl(string $separator = ',', string $enclosure = '"', string $escape = '\\'): ?void {}
public function getCsvControl(): ?array {}
public function flock(int $operation, $wouldBlock = NULL): ?bool {}
public function fflush(): ?bool {}
public function ftell(): ?int|false {}
public function fseek(int $offset, int $whence = 0): ?int {}
public function fgetc(): ?string|false {}
public function fpassthru(): ?int {}
public function fscanf(string $format, mixed $vars): ?array|int|null {}
public function fwrite(string $data, int $length = 0): ?int|false {}
public function fstat(): ?array {}
public function ftruncate(int $size): ?bool {}
public function current(): ?array|string|false {}
public function key(): ?int {}
public function next(): ?void {}
public function setFlags(int $flags): ?void {}
public function getFlags(): ?int {}
public function setMaxLineLen(int $maxLength): ?void {}
public function getMaxLineLen(): ?int {}
public function hasChildren(): ?false {}
public function getChildren(): ?null {}
public function seek(int $line): ?void {}
public function getCurrentLine(): ?string {}
public function __toString(): string {}
public function getPath(): ?string {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isWritable(): ?bool {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class SplDoublyLinkedList {
public function add(int $index, mixed $value): ?void {}
public function pop(): ?mixed {}
public function shift(): ?mixed {}
public function push(mixed $value): ?void {}
public function unshift(mixed $value): ?void {}
public function top(): ?mixed {}
public function bottom(): ?mixed {}
public function __debugInfo(): ?array {}
public function count(): ?int {}
public function isEmpty(): ?bool {}
public function setIteratorMode(int $mode): ?int {}
public function getIteratorMode(): ?int {}
public function offsetExists($index): ?bool {}
public function offsetGet($index): ?mixed {}
public function offsetSet($index, mixed $value): ?void {}
public function offsetUnset($index): ?void {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?int {}
public function prev(): ?void {}
public function next(): ?void {}
public function valid(): ?bool {}
public function unserialize(string $data): ?void {}
public function serialize(): ?string {}
public function __serialize(): ?array {}
public function __unserialize(array $data): ?void {}
}
class SplQueue {
public function enqueue(mixed $value): ?void {}
public function dequeue(): ?mixed {}
public function add(int $index, mixed $value): ?void {}
public function pop(): ?mixed {}
public function shift(): ?mixed {}
public function push(mixed $value): ?void {}
public function unshift(mixed $value): ?void {}
public function top(): ?mixed {}
public function bottom(): ?mixed {}
public function __debugInfo(): ?array {}
public function count(): ?int {}
public function isEmpty(): ?bool {}
public function setIteratorMode(int $mode): ?int {}
public function getIteratorMode(): ?int {}
public function offsetExists($index): ?bool {}
public function offsetGet($index): ?mixed {}
public function offsetSet($index, mixed $value): ?void {}
public function offsetUnset($index): ?void {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?int {}
public function prev(): ?void {}
public function next(): ?void {}
public function valid(): ?bool {}
public function unserialize(string $data): ?void {}
public function serialize(): ?string {}
public function __serialize(): ?array {}
public function __unserialize(array $data): ?void {}
}
class SplStack {
public function add(int $index, mixed $value): ?void {}
public function pop(): ?mixed {}
public function shift(): ?mixed {}
public function push(mixed $value): ?void {}
public function unshift(mixed $value): ?void {}
public function top(): ?mixed {}
public function bottom(): ?mixed {}
public function __debugInfo(): ?array {}
public function count(): ?int {}
public function isEmpty(): ?bool {}
public function setIteratorMode(int $mode): ?int {}
public function getIteratorMode(): ?int {}
public function offsetExists($index): ?bool {}
public function offsetGet($index): ?mixed {}
public function offsetSet($index, mixed $value): ?void {}
public function offsetUnset($index): ?void {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?int {}
public function prev(): ?void {}
public function next(): ?void {}
public function valid(): ?bool {}
public function unserialize(string $data): ?void {}
public function serialize(): ?string {}
public function __serialize(): ?array {}
public function __unserialize(array $data): ?void {}
}
abstract class SplHeap {
public function extract(): ?mixed {}
public function insert(mixed $value): ?bool {}
public function top(): ?mixed {}
public function count(): ?int {}
public function isEmpty(): ?bool {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?int {}
public function next(): ?void {}
public function valid(): ?bool {}
public function recoverFromCorruption(): ?bool {}
protected function compare(mixed $value1, mixed $value2): ?int {}
public function isCorrupted(): ?bool {}
public function __debugInfo(): ?array {}
}
class SplMinHeap {
protected function compare(mixed $value1, mixed $value2): ?int {}
public function extract(): ?mixed {}
public function insert(mixed $value): ?bool {}
public function top(): ?mixed {}
public function count(): ?int {}
public function isEmpty(): ?bool {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?int {}
public function next(): ?void {}
public function valid(): ?bool {}
public function recoverFromCorruption(): ?bool {}
public function isCorrupted(): ?bool {}
public function __debugInfo(): ?array {}
}
class SplMaxHeap {
protected function compare(mixed $value1, mixed $value2): ?int {}
public function extract(): ?mixed {}
public function insert(mixed $value): ?bool {}
public function top(): ?mixed {}
public function count(): ?int {}
public function isEmpty(): ?bool {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?int {}
public function next(): ?void {}
public function valid(): ?bool {}
public function recoverFromCorruption(): ?bool {}
public function isCorrupted(): ?bool {}
public function __debugInfo(): ?array {}
}
class SplPriorityQueue {
public function compare(mixed $priority1, mixed $priority2): ?int {}
public function insert(mixed $value, mixed $priority) {}
public function setExtractFlags(int $flags): ?int {}
public function top(): ?mixed {}
public function extract(): ?mixed {}
public function count(): ?int {}
public function isEmpty(): ?bool {}
public function rewind(): ?void {}
public function current(): ?mixed {}
public function key(): ?int {}
public function next(): ?void {}
public function valid(): ?bool {}
public function recoverFromCorruption() {}
public function isCorrupted(): ?bool {}
public function getExtractFlags(): ?int {}
public function __debugInfo(): ?array {}
}
class SplFixedArray {
public function __construct(int $size = 0) {}
public function __wakeup(): ?void {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
public function count(): ?int {}
public function toArray(): ?array {}
public static function fromArray(array $array, bool $preserveKeys = true): ?SplFixedArray {}
public function getSize(): ?int {}
public function setSize(int $size) {}
public function offsetExists($index): ?bool {}
public function offsetGet($index): ?mixed {}
public function offsetSet($index, mixed $value): ?void {}
public function offsetUnset($index): ?void {}
public function getIterator(): Iterator {}
public function jsonSerialize(): array {}
}
class SplObjectStorage {
public function attach(object $object, mixed $info = NULL): ?void {}
public function detach(object $object): ?void {}
public function contains(object $object): ?bool {}
public function addAll(SplObjectStorage $storage): ?int {}
public function removeAll(SplObjectStorage $storage): ?int {}
public function removeAllExcept(SplObjectStorage $storage): ?int {}
public function getInfo(): ?mixed {}
public function setInfo(mixed $info): ?void {}
public function count(int $mode = 0): ?int {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function key(): ?int {}
public function current(): ?object {}
public function next(): ?void {}
public function unserialize(string $data): ?void {}
public function serialize(): ?string {}
public function offsetExists($object): ?bool {}
public function offsetGet($object): ?mixed {}
public function offsetSet($object, mixed $info = NULL): ?void {}
public function offsetUnset($object): ?void {}
public function getHash(object $object): ?string {}
public function __serialize(): ?array {}
public function __unserialize(array $data): ?void {}
public function __debugInfo(): ?array {}
}
class MultipleIterator {
public function __construct(int $flags = 1) {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function attachIterator(Iterator $iterator, string|int|null $info = NULL): ?void {}
public function detachIterator(Iterator $iterator): ?void {}
public function containsIterator(Iterator $iterator): ?bool {}
public function countIterators(): ?int {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function key(): ?array {}
public function current(): ?array {}
public function next(): ?void {}
public function __debugInfo(): ?array {}
}
class SessionHandler {
public function open(string $path, string $name): ?bool {}
public function close(): ?bool {}
public function read(string $id): ?string|false {}
public function write(string $id, string $data): ?bool {}
public function destroy(string $id): ?bool {}
public function gc(int $max_lifetime): ?int|false {}
public function create_sid(): ?string {}
}
final class __PHP_Incomplete_Class {
}
class AssertionError {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class php_user_filter {
public function filter($in, $out, $consumed, bool $closing): ?int {}
public function onCreate(): ?bool {}
public function onClose(): ?void {}
}
class Directory {
public function close(): ?void {}
public function rewind(): ?void {}
public function read(): ?string|false {}
}
class SodiumException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class PDOException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class PDO {
public function __construct(string $dsn, ?string $username = NULL, ?string $password = NULL, ?array $options = NULL) {}
public function beginTransaction(): ?bool {}
public function commit(): ?bool {}
public function errorCode(): ??string {}
public function errorInfo(): ?array {}
public function exec(string $statement): ?int|false {}
public function getAttribute(int $attribute): ?mixed {}
public static function getAvailableDrivers(): ?array {}
public function inTransaction(): ?bool {}
public function lastInsertId(?string $name = NULL): ?string|false {}
public function prepare(string $query, array $options = array (
)): ?PDOStatement|false {}
public function query(string $query, ?int $fetchMode = NULL, mixed $fetchModeArgs): ?PDOStatement|false {}
public function quote(string $string, int $type = 2): ?string|false {}
public function rollBack(): ?bool {}
public function setAttribute(int $attribute, mixed $value): ?bool {}
}
class PDOStatement {
public function bindColumn(string|int $column, mixed $var, int $type = 2, int $maxLength = 0, mixed $driverOptions = NULL): ?bool {}
public function bindParam(string|int $param, mixed $var, int $type = 2, int $maxLength = 0, mixed $driverOptions = NULL): ?bool {}
public function bindValue(string|int $param, mixed $value, int $type = 2): ?bool {}
public function closeCursor(): ?bool {}
public function columnCount(): ?int {}
public function debugDumpParams(): ??bool {}
public function errorCode(): ??string {}
public function errorInfo(): ?array {}
public function execute(?array $params = NULL): ?bool {}
public function fetch(int $mode = 0, int $cursorOrientation = 0, int $cursorOffset = 0): ?mixed {}
public function fetchAll(int $mode = 0, mixed $args): ?array {}
public function fetchColumn(int $column = 0): ?mixed {}
public function fetchObject(?string $class = 'stdClass', array $constructorArgs = array (
)): ?object|false {}
public function getAttribute(int $name): ?mixed {}
public function getColumnMeta(int $column): ?array|false {}
public function nextRowset(): ?bool {}
public function rowCount(): ?int {}
public function setAttribute(int $attribute, mixed $value): ?bool {}
public function setFetchMode(int $mode, mixed $args) {}
public function getIterator(): Iterator {}
}
final class PDORow {
}
final class XMLParser {
}
class AMQPConnection {
public function __construct(array $credentials) {}
public function isConnected() {}
public function connect() {}
public function pconnect() {}
public function pdisconnect() {}
public function disconnect() {}
public function reconnect() {}
public function preconnect() {}
public function getLogin() {}
public function setLogin($login) {}
public function getPassword() {}
public function setPassword($password) {}
public function getHost() {}
public function setHost($host) {}
public function getPort() {}
public function setPort($port) {}
public function getVhost() {}
public function setVhost($vhost) {}
public function getTimeout() {}
public function setTimeout($timeout) {}
public function getReadTimeout() {}
public function setReadTimeout($timeout) {}
public function getWriteTimeout() {}
public function setWriteTimeout($timeout) {}
public function getRpcTimeout() {}
public function setRpcTimeout($timeout) {}
public function getUsedChannels() {}
public function getMaxChannels() {}
public function isPersistent() {}
public function getHeartbeatInterval() {}
public function getMaxFrameSize() {}
public function getCACert() {}
public function setCACert($cacert) {}
public function getCert() {}
public function setCert($cert) {}
public function getKey() {}
public function setKey($key) {}
public function getVerify() {}
public function setVerify($verify) {}
public function getSaslMethod() {}
public function setSaslMethod($sasl_method) {}
public function getConnectionName() {}
public function setConnectionName($connection_name) {}
}
class AMQPChannel {
public function __construct(AMQPConnection $amqp_connection) {}
public function isConnected() {}
public function close() {}
public function getChannelId() {}
public function setPrefetchSize($size) {}
public function getPrefetchSize() {}
public function setPrefetchCount($count) {}
public function getPrefetchCount() {}
public function setGlobalPrefetchSize($size) {}
public function getGlobalPrefetchSize() {}
public function setGlobalPrefetchCount($count) {}
public function getGlobalPrefetchCount() {}
public function qos($size, $count, $global) {}
public function startTransaction() {}
public function commitTransaction() {}
public function rollbackTransaction() {}
public function getConnection() {}
public function basicRecover($requeue) {}
public function confirmSelect() {}
public function waitForConfirm($timeout) {}
public function setConfirmCallback($ack_callback, $nack_callback) {}
public function setReturnCallback($return_callback) {}
public function waitForBasicReturn($timeout) {}
public function getConsumers() {}
}
class AMQPQueue {
public function __construct(AMQPChannel $amqp_channel) {}
public function getName() {}
public function setName($queue_name) {}
public function getFlags() {}
public function setFlags($flags) {}
public function getArgument($argument) {}
public function getArguments() {}
public function setArgument($key, $value) {}
public function setArguments(array $arguments) {}
public function hasArgument($key) {}
public function declareQueue() {}
public function bind($exchange_name, $routing_key, $arguments) {}
public function get($flags) {}
public function consume($callback, $flags, $consumer_tag) {}
public function ack($delivery_tag, $flags) {}
public function nack($delivery_tag, $flags) {}
public function reject($delivery_tag, $flags) {}
public function purge() {}
public function cancel($consumer_tag) {}
public function delete($flags) {}
public function unbind($exchange_name, $routing_key, $arguments) {}
public function getChannel() {}
public function getConnection() {}
public function getConsumerTag() {}
public function declare() {}
}
class AMQPExchange {
public function __construct(AMQPChannel $amqp_channel) {}
public function getName() {}
public function setName($exchange_name) {}
public function getFlags() {}
public function setFlags($flags) {}
public function getType() {}
public function setType($exchange_type) {}
public function getArgument($argument) {}
public function getArguments() {}
public function setArgument($key, $value) {}
public function setArguments(array $arguments) {}
public function hasArgument($argument) {}
public function declareExchange() {}
public function bind($exchange_name, $routing_key, $flags) {}
public function unbind($exchange_name, $routing_key, $flags) {}
public function delete($exchange_name, $flags) {}
public function publish($message, $routing_key, $flags, array $headers) {}
public function getChannel() {}
public function getConnection() {}
public function declare() {}
}
class AMQPBasicProperties {
public function __construct() {}
public function getContentType() {}
public function getContentEncoding() {}
public function getHeaders() {}
public function getDeliveryMode() {}
public function getPriority() {}
public function getCorrelationId() {}
public function getReplyTo() {}
public function getExpiration() {}
public function getMessageId() {}
public function getTimestamp() {}
public function getType() {}
public function getUserId() {}
public function getAppId() {}
public function getClusterId() {}
}
class AMQPEnvelope {
public function __construct() {}
public function getBody() {}
public function getRoutingKey() {}
public function getConsumerTag() {}
public function getDeliveryTag() {}
public function getExchangeName() {}
public function isRedelivery() {}
public function getHeader($name) {}
public function hasHeader($name) {}
public function getContentType() {}
public function getContentEncoding() {}
public function getHeaders() {}
public function getDeliveryMode() {}
public function getPriority() {}
public function getCorrelationId() {}
public function getReplyTo() {}
public function getExpiration() {}
public function getMessageId() {}
public function getTimestamp() {}
public function getType() {}
public function getUserId() {}
public function getAppId() {}
public function getClusterId() {}
}
final class AMQPTimestamp {
public function __construct($timestamp) {}
public function getTimestamp() {}
public function __toString(): string {}
}
final class AMQPDecimal {
public function __construct($exponent, $significand) {}
public function getExponent() {}
public function getSignificand() {}
}
class AMQPException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class AMQPConnectionException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class AMQPChannelException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class AMQPQueueException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class AMQPEnvelopeException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class AMQPExchangeException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class AMQPValueException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class APCUIterator {
public function __construct($search = NULL, int $format = 4294967295, int $chunk_size = 0, int $list = 1) {}
public function rewind(): void {}
public function next(): void {}
public function valid(): bool {}
public function key(): string|int {}
public function current(): mixed {}
public function getTotalHits(): int {}
public function getTotalSize(): int {}
public function getTotalCount(): int {}
}
final class CurlHandle {
}
final class CurlMultiHandle {
}
final class CurlShareHandle {
}
class CURLFile {
public function __construct(string $filename, ?string $mime_type = NULL, ?string $posted_filename = NULL) {}
public function getFilename(): ?string {}
public function getMimeType(): ?string {}
public function getPostFilename(): ?string {}
public function setMimeType(string $mime_type): ?void {}
public function setPostFilename(string $posted_filename): ?void {}
}
class CURLStringFile {
public function __construct(string $data, string $postname, string $mime = 'application/octet-stream') {}
}
final class DOMException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class DOMImplementation {
public function getFeature(string $feature, string $version): ?never {}
public function hasFeature(string $feature, string $version): ?bool {}
public function createDocumentType(string $qualifiedName, string $publicId = '', string $systemId = '') {}
public function createDocument(?string $namespace = NULL, string $qualifiedName = '', ?DOMDocumentType $doctype = NULL) {}
}
class DOMNode {
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMNameSpaceNode {
}
class DOMDocumentFragment {
public function __construct() {}
public function appendXML(string $data): ?bool {}
public function append($nodes): void {}
public function prepend($nodes): void {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMDocument {
public function __construct(string $version = '1.0', string $encoding = '') {}
public function createAttribute(string $localName) {}
public function createAttributeNS(?string $namespace, string $qualifiedName) {}
public function createCDATASection(string $data) {}
public function createComment(string $data): ?DOMComment {}
public function createDocumentFragment(): ?DOMDocumentFragment {}
public function createElement(string $localName, string $value = '') {}
public function createElementNS(?string $namespace, string $qualifiedName, string $value = '') {}
public function createEntityReference(string $name) {}
public function createProcessingInstruction(string $target, string $data = '') {}
public function createTextNode(string $data): ?DOMText {}
public function getElementById(string $elementId): ??DOMElement {}
public function getElementsByTagName(string $qualifiedName): ?DOMNodeList {}
public function getElementsByTagNameNS(?string $namespace, string $localName): ?DOMNodeList {}
public function importNode(DOMNode $node, bool $deep = false) {}
public function load(string $filename, int $options = 0) {}
public function loadXML(string $source, int $options = 0) {}
public function normalizeDocument(): ?void {}
public function registerNodeClass(string $baseClass, ?string $extendedClass): ?bool {}
public function save(string $filename, int $options = 0): ?int|false {}
public function loadHTML(string $source, int $options = 0) {}
public function loadHTMLFile(string $filename, int $options = 0) {}
public function saveHTML(?DOMNode $node = NULL): ?string|false {}
public function saveHTMLFile(string $filename): ?int|false {}
public function saveXML(?DOMNode $node = NULL, int $options = 0): ?string|false {}
public function schemaValidate(string $filename, int $flags = 0): ?bool {}
public function schemaValidateSource(string $source, int $flags = 0): ?bool {}
public function relaxNGValidate(string $filename): ?bool {}
public function relaxNGValidateSource(string $source): ?bool {}
public function validate(): ?bool {}
public function xinclude(int $options = 0): ?int|false {}
public function adoptNode(DOMNode $node) {}
public function append($nodes): void {}
public function prepend($nodes): void {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMNodeList {
public function count(): ?int {}
public function getIterator(): Iterator {}
public function item(int $index) {}
}
class DOMNamedNodeMap {
public function getNamedItem(string $qualifiedName): ??DOMNode {}
public function getNamedItemNS(?string $namespace, string $localName): ??DOMNode {}
public function item(int $index): ??DOMNode {}
public function count(): ?int {}
public function getIterator(): Iterator {}
}
class DOMCharacterData {
public function appendData(string $data): ?bool {}
public function substringData(int $offset, int $count) {}
public function insertData(int $offset, string $data): ?bool {}
public function deleteData(int $offset, int $count): ?bool {}
public function replaceData(int $offset, int $count, string $data): ?bool {}
public function replaceWith($nodes): void {}
public function remove(): void {}
public function before($nodes): void {}
public function after($nodes): void {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMAttr {
public function __construct(string $name, string $value = '') {}
public function isId(): ?bool {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMElement {
public function __construct(string $qualifiedName, ?string $value = NULL, string $namespace = '') {}
public function getAttribute(string $qualifiedName): ?string {}
public function getAttributeNS(?string $namespace, string $localName): ?string {}
public function getAttributeNode(string $qualifiedName) {}
public function getAttributeNodeNS(?string $namespace, string $localName) {}
public function getElementsByTagName(string $qualifiedName): ?DOMNodeList {}
public function getElementsByTagNameNS(?string $namespace, string $localName): ?DOMNodeList {}
public function hasAttribute(string $qualifiedName): ?bool {}
public function hasAttributeNS(?string $namespace, string $localName): ?bool {}
public function removeAttribute(string $qualifiedName): ?bool {}
public function removeAttributeNS(?string $namespace, string $localName): ?void {}
public function removeAttributeNode(DOMAttr $attr) {}
public function setAttribute(string $qualifiedName, string $value) {}
public function setAttributeNS(?string $namespace, string $qualifiedName, string $value): ?void {}
public function setAttributeNode(DOMAttr $attr) {}
public function setAttributeNodeNS(DOMAttr $attr) {}
public function setIdAttribute(string $qualifiedName, bool $isId): ?void {}
public function setIdAttributeNS(string $namespace, string $qualifiedName, bool $isId): ?void {}
public function setIdAttributeNode(DOMAttr $attr, bool $isId): ?void {}
public function remove(): void {}
public function before($nodes): void {}
public function after($nodes): void {}
public function replaceWith($nodes): void {}
public function append($nodes): void {}
public function prepend($nodes): void {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMText {
public function __construct(string $data = '') {}
public function isWhitespaceInElementContent(): ?bool {}
public function isElementContentWhitespace(): ?bool {}
public function splitText(int $offset) {}
public function appendData(string $data): ?bool {}
public function substringData(int $offset, int $count) {}
public function insertData(int $offset, string $data): ?bool {}
public function deleteData(int $offset, int $count): ?bool {}
public function replaceData(int $offset, int $count, string $data): ?bool {}
public function replaceWith($nodes): void {}
public function remove(): void {}
public function before($nodes): void {}
public function after($nodes): void {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMComment {
public function __construct(string $data = '') {}
public function appendData(string $data): ?bool {}
public function substringData(int $offset, int $count) {}
public function insertData(int $offset, string $data): ?bool {}
public function deleteData(int $offset, int $count): ?bool {}
public function replaceData(int $offset, int $count, string $data): ?bool {}
public function replaceWith($nodes): void {}
public function remove(): void {}
public function before($nodes): void {}
public function after($nodes): void {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMCdataSection {
public function __construct(string $data) {}
public function isWhitespaceInElementContent(): ?bool {}
public function isElementContentWhitespace(): ?bool {}
public function splitText(int $offset) {}
public function appendData(string $data): ?bool {}
public function substringData(int $offset, int $count) {}
public function insertData(int $offset, string $data): ?bool {}
public function deleteData(int $offset, int $count): ?bool {}
public function replaceData(int $offset, int $count, string $data): ?bool {}
public function replaceWith($nodes): void {}
public function remove(): void {}
public function before($nodes): void {}
public function after($nodes): void {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMDocumentType {
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMNotation {
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMEntity {
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMEntityReference {
public function __construct(string $name) {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMProcessingInstruction {
public function __construct(string $name, string $value = '') {}
public function appendChild(DOMNode $node) {}
public function C14N(bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?string|false {}
public function C14NFile(string $uri, bool $exclusive = false, bool $withComments = false, ?array $xpath = NULL, ?array $nsPrefixes = NULL): ?int|false {}
public function cloneNode(bool $deep = false) {}
public function getLineNo(): ?int {}
public function getNodePath(): ??string {}
public function hasAttributes(): ?bool {}
public function hasChildNodes(): ?bool {}
public function insertBefore(DOMNode $node, ?DOMNode $child = NULL) {}
public function isDefaultNamespace(string $namespace): ?bool {}
public function isSameNode(DOMNode $otherNode): ?bool {}
public function isSupported(string $feature, string $version): ?bool {}
public function lookupNamespaceURI(?string $prefix): ??string {}
public function lookupPrefix(string $namespace): ??string {}
public function normalize(): ?void {}
public function removeChild(DOMNode $child) {}
public function replaceChild(DOMNode $node, DOMNode $child) {}
}
class DOMXPath {
public function __construct(DOMDocument $document, bool $registerNodeNS = true) {}
public function evaluate(string $expression, ?DOMNode $contextNode = NULL, bool $registerNodeNS = true): ?mixed {}
public function query(string $expression, ?DOMNode $contextNode = NULL, bool $registerNodeNS = true): ?mixed {}
public function registerNamespace(string $prefix, string $namespace): ?bool {}
public function registerPhpFunctions(array|string|null $restrict = NULL): ?void {}
}
namespace FFI {
class Exception {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
}
namespace FFI {
final class ParserException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
}
final class FFI {
public static function cdef(string $code = '', ?string $lib = NULL): FFI {}
public static function load(string $filename): ?FFI {}
public static function scope(string $name): FFI {}
public static function new(FFI\CType|string $type, bool $owned = true, bool $persistent = false): ?FFI\CData {}
public static function free(FFI\CData $ptr): void {}
public static function cast(FFI\CType|string $type, $ptr): ?FFI\CData {}
public static function type(string $type): ?FFI\CType {}
public static function typeof(FFI\CData $ptr): FFI\CType {}
public static function arrayType(FFI\CType $type, array $dimensions): FFI\CType {}
public static function addr(FFI\CData $ptr): FFI\CData {}
public static function sizeof(FFI\CData|FFI\CType $ptr): int {}
public static function alignof(FFI\CData|FFI\CType $ptr): int {}
public static function memcpy(FFI\CData $to, $from, int $size): void {}
public static function memcmp($ptr1, $ptr2, int $size): int {}
public static function memset(FFI\CData $ptr, int $value, int $size): void {}
public static function string(FFI\CData $ptr, ?int $size = NULL): string {}
public static function isNull(FFI\CData $ptr): bool {}
}
namespace FFI {
final class CData {
}
}
namespace FFI {
final class CType {
public function getName(): string {}
public function getKind(): int {}
public function getSize(): int {}
public function getAlignment(): int {}
public function getAttributes(): int {}
public function getEnumKind(): int {}
public function getArrayElementType(): FFI\CType {}
public function getArrayLength(): int {}
public function getPointerType(): FFI\CType {}
public function getStructFieldNames(): array {}
public function getStructFieldOffset(string $name): int {}
public function getStructFieldType(string $name): FFI\CType {}
public function getFuncABI(): int {}
public function getFuncReturnType(): FFI\CType {}
public function getFuncParameterCount(): int {}
public function getFuncParameterType(int $index): FFI\CType {}
}
}
class finfo {
public function __construct(int $flags = 0, ?string $magic_database = NULL) {}
public function file(string $filename, int $flags = 0, $context = NULL): ?string|false {}
public function buffer(string $string, int $flags = 0, $context = NULL): ?string|false {}
public function set_flags(int $flags) {}
}
namespace FTP {
final class Connection {
}
}
final class GdImage {
}
final class GdFont {
}
class GMP {
public function __construct(string|int $num = 0, int $base = 0) {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
}
class ImagickException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ImagickDrawException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ImagickPixelIteratorException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ImagickPixelException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class ImagickKernelException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class Imagick {
public function optimizeImageLayers(): bool {}
public function compareImageLayers(int $metric): Imagick {}
public function pingImageBlob(string $image): bool {}
public function pingImageFile(mixed $filehandle, ?string $filename = NULL): bool {}
public function transposeImage(): bool {}
public function transverseImage(): bool {}
public function trimImage(float $fuzz): bool {}
public function waveImage(float $amplitude, float $length): bool {}
public function vignetteImage(float $black_point, float $white_point, int $x, int $y): bool {}
public function uniqueImageColors(): bool {}
public function getImageMatte(): bool {}
public function setImageMatte(bool $matte): bool {}
public function adaptiveResizeImage(int $columns, int $rows, bool $bestfit = false, bool $legacy = false): bool {}
public function sketchImage(float $radius, float $sigma, float $angle): bool {}
public function shadeImage(bool $gray, float $azimuth, float $elevation): bool {}
public function getSizeOffset(): int {}
public function setSizeOffset(int $columns, int $rows, int $offset): bool {}
public function adaptiveBlurImage(float $radius, float $sigma, int $channel = 134217719): bool {}
public function contrastStretchImage(float $black_point, float $white_point, int $channel = 134217719): bool {}
public function adaptiveSharpenImage(float $radius, float $sigma, int $channel = 134217719): bool {}
public function randomThresholdImage(float $low, float $high, int $channel = 134217719): bool {}
public function roundCornersImage(float $x_rounding, float $y_rounding, float $stroke_width = 10, float $displace = 5, float $size_correction = -6): bool {}
public function roundCorners(float $x_rounding, float $y_rounding, float $stroke_width = 10, float $displace = 5, float $size_correction = -6): bool {}
public function setIteratorIndex(int $index): bool {}
public function getIteratorIndex(): int {}
public function transformImage(string $crop, string $geometry): Imagick {}
public function setImageOpacity(float $opacity): bool {}
public function orderedPosterizeImage(string $threshold_map, int $channel = 134217719): bool {}
public function polaroidImage(ImagickDraw $settings, float $angle): bool {}
public function getImageProperty(string $name): string {}
public function setImageProperty(string $name, string $value): bool {}
public function deleteImageProperty(string $name): bool {}
public function identifyFormat(string $format): string {}
public function setImageInterpolateMethod(int $method): bool {}
public function getImageInterpolateMethod(): int {}
public function linearStretchImage(float $black_point, float $white_point): bool {}
public function getImageLength(): int {}
public function extentImage(int $width, int $height, int $x, int $y): bool {}
public function getImageOrientation(): int {}
public function setImageOrientation(int $orientation): bool {}
public function paintFloodfillImage(ImagickPixel|string $fill_color, float $fuzz, ImagickPixel|string $border_color, int $x, int $y, int $channel = 134217719): bool {}
public function clutImage(Imagick $lookup_table, int $channel = 134217719): bool {}
public function getImageProperties(string $pattern = '*', bool $include_values = true): array {}
public function getImageProfiles(string $pattern = '*', bool $include_values = true): array {}
public function distortImage(int $distortion, array $arguments, bool $bestfit): bool {}
public function writeImageFile(mixed $filehandle, ?string $format = NULL): bool {}
public function writeImagesFile(mixed $filehandle, ?string $format = NULL): bool {}
public function resetImagePage(string $page): bool {}
public function setImageClipMask(imagick $clip_mask): bool {}
public function getImageClipMask(): Imagick {}
public function animateImages(string $x_server): bool {}
public function recolorImage(array $matrix): bool {}
public function setFont(string $font): bool {}
public function getFont(): string {}
public function setPointSize(float $point_size): bool {}
public function getPointSize(): float {}
public function mergeImageLayers(int $layermethod): Imagick {}
public function setImageAlphaChannel(int $alphachannel): bool {}
public function floodfillPaintImage(ImagickPixel|string $fill_color, float $fuzz, ImagickPixel|string $border_color, int $x, int $y, bool $invert, ?int $channel = 134217719): bool {}
public function opaquePaintImage(ImagickPixel|string $target_color, ImagickPixel|string $fill_color, float $fuzz, bool $invert, int $channel = 134217719): bool {}
public function transparentPaintImage(ImagickPixel|string $target_color, float $alpha, float $fuzz, bool $invert): bool {}
public function liquidRescaleImage(int $width, int $height, float $delta_x, float $rigidity): bool {}
public function encipherImage(string $passphrase): bool {}
public function decipherImage(string $passphrase): bool {}
public function setGravity(int $gravity): bool {}
public function getGravity(): int {}
public function getImageChannelRange(int $channel): array {}
public function getImageAlphaChannel(): bool {}
public function getImageChannelDistortions(Imagick $reference_image, int $metric, int $channel = 134217719): float {}
public function setImageGravity(int $gravity): bool {}
public function getImageGravity(): int {}
public function importImagePixels(int $x, int $y, int $width, int $height, string $map, int $pixelstorage, array $pixels): bool {}
public function deskewImage(float $threshold): bool {}
public function segmentImage(int $colorspace, float $cluster_threshold, float $smooth_threshold, bool $verbose = false): bool {}
public function sparseColorImage(int $sparsecolormethod, array $arguments, int $channel = 134217719): bool {}
public function remapImage(Imagick $replacement, int $dither_method): bool {}
public function exportImagePixels(int $x, int $y, int $width, int $height, string $map, int $pixelstorage): array {}
public function getImageChannelKurtosis(int $channel = 134217719): array {}
public function functionImage(int $function, array $parameters, int $channel = 134217719): bool {}
public function transformImageColorspace(int $colorspace): bool {}
public function haldClutImage(Imagick $clut, int $channel = 134217719): bool {}
public function autoLevelImage(int $channel = 134217719): bool {}
public function blueShiftImage(float $factor = 1.5): bool {}
public function getImageArtifact(string $artifact): ?string {}
public function setImageArtifact(string $artifact, ?string $value): bool {}
public function deleteImageArtifact(string $artifact): bool {}
public function getColorspace(): int {}
public function setColorspace(int $colorspace): bool {}
public function clampImage(int $channel = 134217719): bool {}
public function smushImages(bool $stack, int $offset): Imagick {}
public function __construct(array|string|int|float|null $files = NULL) {}
public function __toString(): string {}
public function count(int $mode = 0): int {}
public function getPixelIterator(): ImagickPixelIterator {}
public function getPixelRegionIterator(int $x, int $y, int $columns, int $rows): ImagickPixelIterator {}
public function readImage(string $filename): bool {}
public function readImages(array $filenames): bool {}
public function readImageBlob(string $image, ?string $filename = NULL): bool {}
public function setImageFormat(string $format): bool {}
public function scaleImage(int $columns, int $rows, bool $bestfit = false, bool $legacy = false): bool {}
public function writeImage(?string $filename = NULL): bool {}
public function writeImages(string $filename, bool $adjoin): bool {}
public function blurImage(float $radius, float $sigma, int $channel = 134217719): bool {}
public function thumbnailImage(?int $columns, ?int $rows, bool $bestfit = false, bool $fill = false, bool $legacy = false): bool {}
public function cropThumbnailImage(int $width, int $height, bool $legacy = false): bool {}
public function getImageFilename(): string {}
public function setImageFilename(string $filename): bool {}
public function getImageFormat(): string {}
public function getImageMimeType(): string {}
public function removeImage(): bool {}
public function destroy(): bool {}
public function clear(): bool {}
public function clone(): Imagick {}
public function getImageSize(): int {}
public function getImageBlob(): string {}
public function getImagesBlob(): string {}
public function setFirstIterator(): bool {}
public function setLastIterator(): bool {}
public function resetIterator(): void {}
public function previousImage(): bool {}
public function nextImage(): bool {}
public function hasPreviousImage(): bool {}
public function hasNextImage(): bool {}
public function setImageIndex(int $index): bool {}
public function getImageIndex(): int {}
public function commentImage(string $comment): bool {}
public function cropImage(int $width, int $height, int $x, int $y): bool {}
public function labelImage(string $label): bool {}
public function getImageGeometry(): array {}
public function drawImage(ImagickDraw $drawing): bool {}
public function setImageCompressionQuality(int $quality): bool {}
public function getImageCompressionQuality(): int {}
public function setImageCompression(int $compression): bool {}
public function getImageCompression(): int {}
public function annotateImage(ImagickDraw $settings, float $x, float $y, float $angle, string $text): bool {}
public function compositeImage(Imagick $composite_image, int $composite, int $x, int $y, int $channel = 134217719): bool {}
public function modulateImage(float $brightness, float $saturation, float $hue): bool {}
public function getImageColors(): int {}
public function montageImage(ImagickDraw $settings, string $tile_geometry, string $thumbnail_geometry, int $monatgemode, string $frame): Imagick {}
public function identifyImage(bool $append_raw_output = false): array {}
public function thresholdImage(float $threshold, int $channel = 134217719): bool {}
public function adaptiveThresholdImage(int $width, int $height, int $offset): bool {}
public function blackThresholdImage(ImagickPixel|string $threshold_color): bool {}
public function whiteThresholdImage(ImagickPixel|string $threshold_color): bool {}
public function appendImages(bool $stack): Imagick {}
public function charcoalImage(float $radius, float $sigma): bool {}
public function normalizeImage(int $channel = 134217719): bool {}
public function oilPaintImage(float $radius): bool {}
public function posterizeImage(int $levels, bool $dither): bool {}
public function radialBlurImage(float $angle, int $channel = 134217719): bool {}
public function raiseImage(int $width, int $height, int $x, int $y, bool $raise): bool {}
public function resampleImage(float $x_resolution, float $y_resolution, int $filter, float $blur): bool {}
public function resizeImage(int $columns, int $rows, int $filter, float $blur, bool $bestfit = false, bool $legacy = false): bool {}
public function rollImage(int $x, int $y): bool {}
public function rotateImage(ImagickPixel|string $background_color, float $degrees): bool {}
public function sampleImage(int $columns, int $rows): bool {}
public function solarizeImage(int $threshold): bool {}
public function shadowImage(float $opacity, float $sigma, int $x, int $y): bool {}
public function setImageAttribute(string $key, string $value): bool {}
public function setImageBackgroundColor(ImagickPixel|string $background_color): bool {}
public function setImageCompose(int $compose): bool {}
public function setImageDelay(int $delay): bool {}
public function setImageDepth(int $depth): bool {}
public function setImageGamma(float $gamma): bool {}
public function setImageIterations(int $iterations): bool {}
public function setImageMatteColor(ImagickPixel|string $matte_color): bool {}
public function setImagePage(int $width, int $height, int $x, int $y): bool {}
public function setImageProgressMonitor(string $filename): bool {}
public function setProgressMonitor(callable $callback): bool {}
public function setImageResolution(float $x_resolution, float $y_resolution): bool {}
public function setImageScene(int $scene): bool {}
public function setImageTicksPerSecond(int $ticks_per_second): bool {}
public function setImageType(int $image_type): bool {}
public function setImageUnits(int $units): bool {}
public function sharpenImage(float $radius, float $sigma, int $channel = 134217719): bool {}
public function shaveImage(int $columns, int $rows): bool {}
public function shearImage(ImagickPixel|string $background_color, float $x_shear, float $y_shear): bool {}
public function spliceImage(int $width, int $height, int $x, int $y): bool {}
public function pingImage(string $filename): bool {}
public function readImageFile(mixed $filehandle, ?string $filename = NULL): bool {}
public function displayImage(string $servername): bool {}
public function displayImages(string $servername): bool {}
public function spreadImage(float $radius): bool {}
public function swirlImage(float $degrees): bool {}
public function stripImage(): bool {}
public static function queryFormats(string $pattern = '*'): array {}
public static function queryFonts(string $pattern = '*'): array {}
public function queryFontMetrics(ImagickDraw $settings, string $text, ?bool $multiline = NULL): array {}
public function steganoImage(Imagick $watermark, int $offset): Imagick {}
public function addNoiseImage(int $noise, int $channel = 134217719): bool {}
public function motionBlurImage(float $radius, float $sigma, float $angle, int $channel = 134217719): bool {}
public function mosaicImages(): Imagick {}
public function morphImages(int $number_frames): Imagick {}
public function minifyImage(): bool {}
public function affineTransformImage(ImagickDraw $settings): bool {}
public function averageImages(): Imagick {}
public function borderImage(ImagickPixel|string $border_color, int $width, int $height): bool {}
public static function calculateCrop(int $original_width, int $original_height, int $desired_width, int $desired_height, bool $legacy = false): array {}
public function chopImage(int $width, int $height, int $x, int $y): bool {}
public function clipImage(): bool {}
public function clipPathImage(string $pathname, bool $inside): bool {}
public function clipImagePath(string $pathname, bool $inside): void {}
public function coalesceImages(): Imagick {}
public function colorFloodfillImage(ImagickPixel|string $fill_color, float $fuzz, ImagickPixel|string $border_color, int $x, int $y): bool {}
public function colorizeImage(ImagickPixel|string $colorize_color, ImagickPixel|string|false $opacity_color, ?bool $legacy = false): bool {}
public function compareImageChannels(Imagick $reference, int $channel, int $metric): array {}
public function compareImages(Imagick $reference, int $metric): array {}
public function contrastImage(bool $sharpen): bool {}
public function combineImages(int $colorspace): Imagick {}
public function convolveImage(array $kernel, int $channel = 134217719): bool {}
public function cycleColormapImage(int $displace): bool {}
public function deconstructImages(): Imagick {}
public function despeckleImage(): bool {}
public function edgeImage(float $radius): bool {}
public function embossImage(float $radius, float $sigma): bool {}
public function enhanceImage(): bool {}
public function equalizeImage(): bool {}
public function evaluateImage(int $evaluate, float $constant, int $channel = 134217719): bool {}
public function evaluateImages(int $evaluate): bool {}
public function flattenImages(): Imagick {}
public function flipImage(): bool {}
public function flopImage(): bool {}
public function forwardFourierTransformImage(bool $magnitude): bool {}
public function frameImage(ImagickPixel|string $matte_color, int $width, int $height, int $inner_bevel, int $outer_bevel): bool {}
public function fxImage(string $expression, int $channel = 134217719): Imagick {}
public function gammaImage(float $gamma, int $channel = 134217719): bool {}
public function gaussianBlurImage(float $radius, float $sigma, int $channel = 134217719): bool {}
public function getImageAttribute(string $key): string {}
public function getImageBackgroundColor(): ImagickPixel {}
public function getImageBluePrimary(): array {}
public function getImageBorderColor(): ImagickPixel {}
public function getImageChannelDepth(int $channel): int {}
public function getImageChannelDistortion(Imagick $reference, int $channel, int $metric): float {}
public function getImageChannelExtrema(int $channel): array {}
public function getImageChannelMean(int $channel): array {}
public function getImageChannelStatistics(): array {}
public function getImageColormapColor(int $index): ImagickPixel {}
public function getImageColorspace(): int {}
public function getImageCompose(): int {}
public function getImageDelay(): int {}
public function getImageDepth(): int {}
public function getImageDistortion(Imagick $reference, int $metric): float {}
public function getImageExtrema(): array {}
public function getImageDispose(): int {}
public function getImageGamma(): float {}
public function getImageGreenPrimary(): array {}
public function getImageHeight(): int {}
public function getImageHistogram(): array {}
public function getImageInterlaceScheme(): int {}
public function getImageIterations(): int {}
public function getImageMatteColor(): ImagickPixel {}
public function getImagePage(): array {}
public function getImagePixelColor(int $x, int $y): ImagickPixel {}
public function getImageProfile(string $name): string {}
public function getImageRedPrimary(): array {}
public function getImageRenderingIntent(): int {}
public function getImageResolution(): array {}
public function getImageScene(): int {}
public function getImageSignature(): string {}
public function getImageTicksPerSecond(): int {}
public function getImageType(): int {}
public function getImageUnits(): int {}
public function getImageVirtualPixelMethod(): int {}
public function getImageWhitePoint(): array {}
public function getImageWidth(): int {}
public function getNumberImages(): int {}
public function getImageTotalInkDensity(): float {}
public function getImageRegion(int $width, int $height, int $x, int $y): Imagick {}
public function implodeImage(float $radius): bool {}
public function inverseFourierTransformImage(Imagick $complement, bool $magnitude): bool {}
public function levelImage(float $black_point, float $gamma, float $white_point, int $channel = 134217719): bool {}
public function magnifyImage(): bool {}
public function mapImage(imagick $map, bool $dither): bool {}
public function matteFloodfillImage(float $alpha, float $fuzz, ImagickPixel|string $border_color, int $x, int $y): bool {}
public function medianFilterImage(float $radius): bool {}
public function negateImage(bool $gray, int $channel = 134217719): bool {}
public function paintOpaqueImage(ImagickPixel|string $target_color, ImagickPixel|string $fill_color, float $fuzz, int $channel = 134217719): bool {}
public function paintTransparentImage(ImagickPixel|string $target_color, float $alpha, float $fuzz): bool {}
public function previewImages(int $preview): bool {}
public function profileImage(string $name, ?string $profile): bool {}
public function quantizeImage(int $number_colors, int $colorspace, int $tree_depth, bool $dither, bool $measure_error): bool {}
public function quantizeImages(int $number_colors, int $colorspace, int $tree_depth, bool $dither, bool $measure_error): bool {}
public function reduceNoiseImage(float $radius): bool {}
public function removeImageProfile(string $name): string {}
public function separateImageChannel(int $channel): bool {}
public function sepiaToneImage(float $threshold): bool {}
public function setImageBias(float $bias): bool {}
public function setImageBiasQuantum(string $bias): void {}
public function setImageBluePrimary(float $x, float $y): bool {}
public function setImageBorderColor(ImagickPixel|string $border_color): bool {}
public function setImageChannelDepth(int $channel, int $depth): bool {}
public function setImageColormapColor(int $index, ImagickPixel|string $color): bool {}
public function setImageColorspace(int $colorspace): bool {}
public function setImageDispose(int $dispose): bool {}
public function setImageExtent(int $columns, int $rows): bool {}
public function setImageGreenPrimary(float $x, float $y): bool {}
public function setImageInterlaceScheme(int $interlace): bool {}
public function setImageProfile(string $name, string $profile): bool {}
public function setImageRedPrimary(float $x, float $y): bool {}
public function setImageRenderingIntent(int $rendering_intent): bool {}
public function setImageVirtualPixelMethod(int $method): bool {}
public function setImageWhitePoint(float $x, float $y): bool {}
public function sigmoidalContrastImage(bool $sharpen, float $alpha, float $beta, int $channel = 134217719): bool {}
public function stereoImage(Imagick $offset_image): bool {}
public function textureImage(Imagick $texture): Imagick {}
public function tintImage(ImagickPixel|string $tint_color, ImagickPixel|string $opacity_color, bool $legacy = false): bool {}
public function unsharpMaskImage(float $radius, float $sigma, float $amount, float $threshold, int $channel = 134217719): bool {}
public function getImage(): Imagick {}
public function addImage(Imagick $image): bool {}
public function setImage(Imagick $image): bool {}
public function newImage(int $columns, int $rows, ImagickPixel|string $background_color, string $format = NULL): bool {}
public function newPseudoImage(int $columns, int $rows, string $pseudo_format): bool {}
public function getCompression(): int {}
public function getCompressionQuality(): int {}
public static function getCopyright(): string {}
public static function getConfigureOptions(string $pattern = '*'): array {}
public static function getFeatures(): string {}
public function getFilename(): string {}
public function getFormat(): string {}
public static function getHomeURL(): string {}
public function getInterlaceScheme(): int {}
public function getOption(string $key): string {}
public static function getPackageName(): string {}
public function getPage(): array {}
public static function getQuantum(): int {}
public static function getHdriEnabled(): bool {}
public static function getQuantumDepth(): array {}
public static function getQuantumRange(): array {}
public static function getReleaseDate(): string {}
public static function getResource(int $type): int {}
public static function getResourceLimit(int $type): int {}
public function getSamplingFactors(): array {}
public function getSize(): array {}
public static function getVersion(): array {}
public function setBackgroundColor(ImagickPixel|string $background_color): bool {}
public function setCompression(int $compression): bool {}
public function setCompressionQuality(int $quality): bool {}
public function setFilename(string $filename): bool {}
public function setFormat(string $format): bool {}
public function setInterlaceScheme(int $interlace): bool {}
public function setOption(string $key, string $value): bool {}
public function setPage(int $width, int $height, int $x, int $y): bool {}
public static function setResourceLimit(int $type, int $limit): bool {}
public function setResolution(float $x_resolution, float $y_resolution): bool {}
public function setSamplingFactors(array $factors): bool {}
public function setSize(int $columns, int $rows): bool {}
public function setType(int $imgtype): bool {}
public function key(): int {}
public function next(): ?void {}
public function rewind(): ?void {}
public function valid(): bool {}
public function current(): Imagick {}
public function brightnessContrastImage(float $brightness, float $contrast, int $channel = 134217719): bool {}
public function colorMatrixImage(array $color_matrix): bool {}
public function selectiveBlurImage(float $radius, float $sigma, float $threshold, int $channel = 134217719): bool {}
public function rotationalBlurImage(float $angle, int $channel = 134217719): bool {}
public function statisticImage(int $type, int $width, int $height, int $channel = 134217719): bool {}
public function subimageMatch(Imagick $image, ?array $offset = NULL, ?float $similarity = NULL, float $threshold = 0.0, int $metric = 0): Imagick {}
public function similarityImage(Imagick $image, ?array $offset = NULL, ?float $similarity = NULL, float $threshold = 0.0, int $metric = 0): Imagick {}
public static function setRegistry(string $key, string $value): bool {}
public static function getRegistry(string $key): string {}
public static function listRegistry(): array {}
public function morphology(int $morphology, int $iterations, ImagickKernel $kernel, int $channel = 134217719): bool {}
public function filter(ImagickKernel $kernel, int $channel = 0): bool {}
public function setAntialias(bool $antialias): void {}
public function getAntialias(): bool {}
public function colorDecisionListImage(string $color_correction_collection): bool {}
public function optimizeImageTransparency(): void {}
public function autoGammaImage(?int $channel = 134217727): void {}
public function autoOrient(): void {}
public function autoOrientate(): void {}
public function compositeImageGravity(Imagick $image, int $composite_constant, int $gravity): bool {}
public function localContrastImage(float $radius, float $strength): void {}
}
class ImagickDraw {
public function resetVectorGraphics(): bool {}
public function getTextKerning(): float {}
public function setTextKerning(float $kerning): bool {}
public function getTextInterwordSpacing(): float {}
public function setTextInterwordSpacing(float $spacing): bool {}
public function getTextInterlineSpacing(): float {}
public function setTextInterlineSpacing(float $spacing): bool {}
public function __construct() {}
public function setFillColor(ImagickPixel|string $fill_color): bool {}
public function setFillAlpha(float $alpha): bool {}
public function setResolution(float $resolution_x, float $resolution_y): bool {}
public function setStrokeColor(ImagickPixel|string $color): bool {}
public function setStrokeAlpha(float $alpha): bool {}
public function setStrokeWidth(float $width): bool {}
public function clear(): bool {}
public function circle(float $origin_x, float $origin_y, float $perimeter_x, float $perimeter_y): bool {}
public function annotation(float $x, float $y, string $text): bool {}
public function setTextAntialias(bool $antialias): bool {}
public function setTextEncoding(string $encoding): bool {}
public function setFont(string $font_name): bool {}
public function setFontFamily(string $font_family): bool {}
public function setFontSize(float $point_size): bool {}
public function setFontStyle(int $style): bool {}
public function setFontWeight(int $weight): bool {}
public function getFont(): string {}
public function getFontFamily(): string {}
public function getFontSize(): float {}
public function getFontStyle(): int {}
public function getFontWeight(): int {}
public function destroy(): bool {}
public function rectangle(float $top_left_x, float $top_left_y, float $bottom_right_x, float $bottom_right_y): bool {}
public function roundRectangle(float $top_left_x, float $top_left_y, float $bottom_right_x, float $bottom_right_y, float $rounding_x, float $rounding_y): bool {}
public function ellipse(float $origin_x, float $origin_y, float $radius_x, float $radius_y, float $angle_start, float $angle_end): bool {}
public function skewX(float $degrees): bool {}
public function skewY(float $degrees): bool {}
public function translate(float $x, float $y): bool {}
public function line(float $start_x, float $start_y, float $end_x, float $end_y): bool {}
public function arc(float $start_x, float $start_y, float $end_x, float $end_y, float $start_angle, float $end_angle): bool {}
public function matte(float $x, float $y, int $paint): bool {}
public function polygon(array $coordinates): bool {}
public function point(float $x, float $y): bool {}
public function getTextDecoration(): int {}
public function getTextEncoding(): string {}
public function getFontStretch(): int {}
public function setFontStretch(int $stretch): bool {}
public function setStrokeAntialias(bool $enabled): bool {}
public function setTextAlignment(int $align): bool {}
public function setTextDecoration(int $decoration): bool {}
public function setTextUnderColor(ImagickPixel|string $under_color): bool {}
public function setViewbox(int $left_x, int $top_y, int $right_x, int $bottom_y): bool {}
public function clone(): ImagickDraw {}
public function affine(array $affine): bool {}
public function bezier(array $coordinates): bool {}
public function composite(int $composite, float $x, float $y, float $width, float $height, Imagick $image): bool {}
public function color(float $x, float $y, int $paint): bool {}
public function comment(string $comment): bool {}
public function getClipPath(): string {}
public function getClipRule(): int {}
public function getClipUnits(): int {}
public function getFillColor(): ImagickPixel {}
public function getFillOpacity(): float {}
public function getFillRule(): int {}
public function getGravity(): int {}
public function getStrokeAntialias(): bool {}
public function getStrokeColor(): ImagickPixel {}
public function getStrokeDashArray(): array {}
public function getStrokeDashOffset(): float {}
public function getStrokeLineCap(): int {}
public function getStrokeLineJoin(): int {}
public function getStrokeMiterLimit(): int {}
public function getStrokeOpacity(): float {}
public function getStrokeWidth(): float {}
public function getTextAlignment(): int {}
public function getTextAntialias(): bool {}
public function getVectorGraphics(): string {}
public function getTextUnderColor(): ImagickPixel {}
public function pathClose(): bool {}
public function pathCurveToAbsolute(float $x1, float $y1, float $x2, float $y2, float $x, float $y): bool {}
public function pathCurveToRelative(float $x1, float $y1, float $x2, float $y2, float $x, float $y): bool {}
public function pathCurveToQuadraticBezierAbsolute(float $x1, float $y1, float $x_end, float $y): bool {}
public function pathCurveToQuadraticBezierRelative(float $x1, float $y1, float $x_end, float $y): bool {}
public function pathCurveToQuadraticBezierSmoothAbsolute(float $x, float $y): bool {}
public function pathCurveToQuadraticBezierSmoothRelative(float $x, float $y): bool {}
public function pathCurveToSmoothAbsolute(float $x2, float $y2, float $x, float $y): bool {}
public function pathCurveToSmoothRelative(float $x2, float $y2, float $x, float $y): bool {}
public function pathEllipticArcAbsolute(float $rx, float $ry, float $x_axis_rotation, bool $large_arc, bool $sweep, float $x, float $y): bool {}
public function pathEllipticArcRelative(float $rx, float $ry, float $x_axis_rotation, bool $large_arc, bool $sweep, float $x, float $y): bool {}
public function pathFinish(): bool {}
public function pathLineToAbsolute(float $x, float $y): bool {}
public function pathLineToRelative(float $x, float $y): bool {}
public function pathLineToHorizontalAbsolute(float $x): bool {}
public function pathLineToHorizontalRelative(float $x): bool {}
public function pathLineToVerticalAbsolute(float $y): bool {}
public function pathLineToVerticalRelative(float $y): bool {}
public function pathMoveToAbsolute(float $x, float $y): bool {}
public function pathMoveToRelative(float $x, float $y): bool {}
public function pathStart(): bool {}
public function polyline(array $coordinates): bool {}
public function popClipPath(): bool {}
public function popDefs(): bool {}
public function popPattern(): bool {}
public function pushClipPath(string $clip_mask_id): bool {}
public function pushDefs(): bool {}
public function pushPattern(string $pattern_id, float $x, float $y, float $width, float $height): bool {}
public function render(): bool {}
public function rotate(float $degrees): bool {}
public function scale(float $x, float $y): bool {}
public function setClipPath(string $clip_mask): bool {}
public function setClipRule(int $fillrule): bool {}
public function setClipUnits(int $pathunits): bool {}
public function setFillOpacity(float $opacity): bool {}
public function setFillPatternUrl(string $fill_url): bool {}
public function setFillRule(int $fillrule): bool {}
public function setGravity(int $gravity): bool {}
public function setStrokePatternUrl(string $stroke_url): bool {}
public function setStrokeDashOffset(float $dash_offset): bool {}
public function setStrokeLineCap(int $linecap): bool {}
public function setStrokeLineJoin(int $linejoin): bool {}
public function setStrokeMiterLimit(int $miterlimit): bool {}
public function setStrokeOpacity(float $opacity): bool {}
public function setVectorGraphics(string $xml): bool {}
public function pop(): bool {}
public function push(): bool {}
public function setStrokeDashArray(array $dashes): bool {}
public function getOpacity(): float {}
public function setOpacity(float $opacity): bool {}
public function getFontResolution(): array {}
public function setFontResolution(float $x, float $y): bool {}
public function getBorderColor(): ImagickPixel {}
public function setBorderColor(ImagickPixel|string $color): bool {}
public function setDensity(string $density): bool {}
public function getDensity(): ?string {}
public function getTextDirection(): int {}
public function setTextDirection(int $direction): bool {}
}
class ImagickPixelIterator {
public function __construct(Imagick $imagick) {}
public function clear(): bool {}
public static function getPixelIterator(Imagick $imagick): ImagickPixelIterator {}
public static function getPixelRegionIterator(Imagick $imagick, int $x, int $y, int $columns, int $rows): ImagickPixelIterator {}
public function destroy(): bool {}
public function getCurrentIteratorRow(): array {}
public function getIteratorRow(): int {}
public function getNextIteratorRow(): array {}
public function getPreviousIteratorRow(): array {}
public function key(): int {}
public function next(): ?void {}
public function rewind(): ?void {}
public function current(): array {}
public function newPixelIterator(Imagick $imagick): bool {}
public function newPixelRegionIterator(Imagick $imagick, int $x, int $y, int $columns, int $rows): bool {}
public function resetIterator(): bool {}
public function setIteratorFirstRow(): bool {}
public function setIteratorLastRow(): bool {}
public function setIteratorRow(int $row): bool {}
public function syncIterator(): bool {}
public function valid(): bool {}
}
class ImagickPixel {
public function __construct(?string $color = NULL) {}
public function clear(): bool {}
public function destroy(): bool {}
public function getColor(int $normalized = 0): array {}
public function getColorAsString(): string {}
public function getColorCount(): int {}
public function getColorQuantum(): array {}
public function getColorValue(int $color): float {}
public function getColorValueQuantum(int $color): int {}
public function getHSL(): array {}
public function getIndex(): int {}
public function isPixelSimilar(ImagickPixel|string $color, float $fuzz): bool {}
public function isPixelSimilarQuantum(ImagickPixel|string $color, float $fuzz_quantum_range_scaled_by_square_root_of_three): bool {}
public function isSimilar(ImagickPixel|string $color, float $fuzz_quantum_range_scaled_by_square_root_of_three): bool {}
public function setColor(string $color): bool {}
public function setColorCount(int $color_count): bool {}
public function setColorValue(int $color, float $value): bool {}
public function setColorValueQuantum(int $color, int $value): bool {}
public function setHSL(float $hue, float $saturation, float $luminosity): bool {}
public function setIndex(int $index): bool {}
public function setColorFromPixel(ImagickPixel $pixel): bool {}
}
class ImagickKernel {
public function addKernel(ImagickKernel $kernel): void {}
public function addUnityKernel(float $scale): void {}
public static function fromBuiltin(int $kernel, string $shape): ImagickKernel {}
public static function fromMatrix(array $matrix, ?array $origin): ImagickKernel {}
public function getMatrix(): array {}
public function scale(float $scale, ?int $normalize_kernel = NULL): void {}
public function separate(): array {}
}
class Collator {
public function __construct(string $locale) {}
public static function create(string $locale): ??Collator {}
public function compare(string $string1, string $string2): ?int|false {}
public function sort(array $array, int $flags = 0): ?bool {}
public function sortWithSortKeys(array $array): ?bool {}
public function asort(array $array, int $flags = 0): ?bool {}
public function getAttribute(int $attribute): ?int|false {}
public function setAttribute(int $attribute, int $value): ?bool {}
public function getStrength(): ?int {}
public function setStrength(int $strength) {}
public function getLocale(int $type): ?string|false {}
public function getErrorCode(): ?int|false {}
public function getErrorMessage(): ?string|false {}
public function getSortKey(string $string): ?string|false {}
}
class NumberFormatter {
public function __construct(string $locale, int $style, ?string $pattern = NULL) {}
public static function create(string $locale, int $style, ?string $pattern = NULL): ??NumberFormatter {}
public function format(int|float $num, int $type = 0): ?string|false {}
public function parse(string $string, int $type = 3, $offset = NULL): ?int|float|false {}
public function formatCurrency(float $amount, string $currency): ?string|false {}
public function parseCurrency(string $string, $currency, $offset = NULL): ?float|false {}
public function setAttribute(int $attribute, int|float $value): ?bool {}
public function getAttribute(int $attribute): ?int|float|false {}
public function setTextAttribute(int $attribute, string $value): ?bool {}
public function getTextAttribute(int $attribute): ?string|false {}
public function setSymbol(int $symbol, string $value): ?bool {}
public function getSymbol(int $symbol): ?string|false {}
public function setPattern(string $pattern): ?bool {}
public function getPattern(): ?string|false {}
public function getLocale(int $type = 0): ?string|false {}
public function getErrorCode(): ?int {}
public function getErrorMessage(): ?string {}
}
class Normalizer {
public static function normalize(string $string, int $form = 16): ?string|false {}
public static function isNormalized(string $string, int $form = 16): ?bool {}
public static function getRawDecomposition(string $string, int $form = 16): ??string {}
}
class Locale {
public static function getDefault(): ?string {}
public static function setDefault(string $locale) {}
public static function getPrimaryLanguage(string $locale): ??string {}
public static function getScript(string $locale): ??string {}
public static function getRegion(string $locale): ??string {}
public static function getKeywords(string $locale): ?array|false|null {}
public static function getDisplayScript(string $locale, ?string $displayLocale = NULL): ?string|false {}
public static function getDisplayRegion(string $locale, ?string $displayLocale = NULL): ?string|false {}
public static function getDisplayName(string $locale, ?string $displayLocale = NULL): ?string|false {}
public static function getDisplayLanguage(string $locale, ?string $displayLocale = NULL): ?string|false {}
public static function getDisplayVariant(string $locale, ?string $displayLocale = NULL): ?string|false {}
public static function composeLocale(array $subtags): ?string|false {}
public static function parseLocale(string $locale): ??array {}
public static function getAllVariants(string $locale): ??array {}
public static function filterMatches(string $languageTag, string $locale, bool $canonicalize = false): ??bool {}
public static function lookup(array $languageTag, string $locale, bool $canonicalize = false, ?string $defaultLocale = NULL): ??string {}
public static function canonicalize(string $locale): ??string {}
public static function acceptFromHttp(string $header): ?string|false {}
}
class MessageFormatter {
public function __construct(string $locale, string $pattern) {}
public static function create(string $locale, string $pattern): ??MessageFormatter {}
public function format(array $values): ?string|false {}
public static function formatMessage(string $locale, string $pattern, array $values): ?string|false {}
public function parse(string $string): ?array|false {}
public static function parseMessage(string $locale, string $pattern, string $message): ?array|false {}
public function setPattern(string $pattern): ?bool {}
public function getPattern(): ?string|false {}
public function getLocale(): ?string {}
public function getErrorCode(): ?int {}
public function getErrorMessage(): ?string {}
}
class IntlDateFormatter {
public function __construct(?string $locale, int $dateType = 0, int $timeType = 0, $timezone = NULL, $calendar = NULL, ?string $pattern = NULL) {}
public static function create(?string $locale, int $dateType = 0, int $timeType = 0, $timezone = NULL, IntlCalendar|int|null $calendar = NULL, ?string $pattern = NULL): ??IntlDateFormatter {}
public function getDateType(): ?int|false {}
public function getTimeType(): ?int|false {}
public function getCalendar(): ?int|false {}
public function setCalendar(IntlCalendar|int|null $calendar): ?bool {}
public function getTimeZoneId(): ?string|false {}
public function getCalendarObject(): ?IntlCalendar|false|null {}
public function getTimeZone(): ?IntlTimeZone|false {}
public function setTimeZone($timezone): ??bool {}
public function setPattern(string $pattern): ?bool {}
public function getPattern(): ?string|false {}
public function getLocale(int $type = 0): ?string|false {}
public function setLenient(bool $lenient): ?void {}
public function isLenient(): ?bool {}
public function format($datetime): ?string|false {}
public static function formatObject($datetime, $format = NULL, ?string $locale = NULL): ?string|false {}
public function parse(string $string, $offset = NULL): ?int|float|false {}
public function localtime(string $string, $offset = NULL): ?array|false {}
public function getErrorCode(): ?int {}
public function getErrorMessage(): ?string {}
}
class IntlDatePatternGenerator {
public function __construct(?string $locale = NULL) {}
public static function create(?string $locale = NULL): ?IntlDatePatternGenerator {}
public function getBestPattern(string $skeleton): string|false {}
}
class ResourceBundle {
public function __construct(?string $locale, ?string $bundle, bool $fallback = true) {}
public static function create(?string $locale, ?string $bundle, bool $fallback = true): ??ResourceBundle {}
public function get($index, bool $fallback = true): ?mixed {}
public function count(): ?int {}
public static function getLocales(string $bundle): ?array|false {}
public function getErrorCode(): ?int {}
public function getErrorMessage(): ?string {}
public function getIterator(): Iterator {}
}
class Transliterator {
final private function __construct() {}
public static function create(string $id, int $direction = 0): ??Transliterator {}
public static function createFromRules(string $rules, int $direction = 0): ??Transliterator {}
public function createInverse(): ??Transliterator {}
public static function listIDs(): ?array|false {}
public function transliterate(string $string, int $start = 0, int $end = -1): ?string|false {}
public function getErrorCode(): ?int|false {}
public function getErrorMessage(): ?string|false {}
}
class IntlTimeZone {
private function __construct() {}
public static function countEquivalentIDs(string $timezoneId): ?int|false {}
public static function createDefault(): ?IntlTimeZone {}
public static function createEnumeration($countryOrRawOffset = NULL): ?IntlIterator|false {}
public static function createTimeZone(string $timezoneId): ??IntlTimeZone {}
public static function createTimeZoneIDEnumeration(int $type, ?string $region = NULL, ?int $rawOffset = NULL): ?IntlIterator|false {}
public static function fromDateTimeZone(DateTimeZone $timezone): ??IntlTimeZone {}
public static function getCanonicalID(string $timezoneId, $isSystemId = NULL): ?string|false {}
public function getDisplayName(bool $dst = false, int $style = 2, ?string $locale = NULL): ?string|false {}
public function getDSTSavings(): ?int {}
public static function getEquivalentID(string $timezoneId, int $offset): ?string|false {}
public function getErrorCode(): ?int|false {}
public function getErrorMessage(): ?string|false {}
public static function getGMT(): ?IntlTimeZone {}
public function getID(): ?string|false {}
public function getOffset(float $timestamp, bool $local, $rawOffset, $dstOffset): ?bool {}
public function getRawOffset(): ?int {}
public static function getRegion(string $timezoneId): ?string|false {}
public static function getTZDataVersion(): ?string|false {}
public static function getUnknown(): ?IntlTimeZone {}
public static function getWindowsID(string $timezoneId): ?string|false {}
public static function getIDForWindowsID(string $timezoneId, ?string $region = NULL): ?string|false {}
public function hasSameRules(IntlTimeZone $other): ?bool {}
public function toDateTimeZone(): ?DateTimeZone|false {}
public function useDaylightTime(): ?bool {}
}
class IntlCalendar {
private function __construct() {}
public static function createInstance($timezone = NULL, ?string $locale = NULL): ??IntlCalendar {}
public function equals(IntlCalendar $other): ?bool {}
public function fieldDifference(float $timestamp, int $field): ?int|false {}
public function add(int $field, int $value): ?bool {}
public function after(IntlCalendar $other): ?bool {}
public function before(IntlCalendar $other): ?bool {}
public function clear(?int $field = NULL) {}
public static function fromDateTime(DateTime|string $datetime, ?string $locale = NULL): ??IntlCalendar {}
public function get(int $field): ?int|false {}
public function getActualMaximum(int $field): ?int|false {}
public function getActualMinimum(int $field): ?int|false {}
public static function getAvailableLocales(): ?array {}
public function getDayOfWeekType(int $dayOfWeek): ?int|false {}
public function getErrorCode(): ?int|false {}
public function getErrorMessage(): ?string|false {}
public function getFirstDayOfWeek(): ?int|false {}
public function getGreatestMinimum(int $field): ?int|false {}
public static function getKeywordValuesForLocale(string $keyword, string $locale, bool $onlyCommon): ?IntlIterator|false {}
public function getLeastMaximum(int $field): ?int|false {}
public function getLocale(int $type): ?string|false {}
public function getMaximum(int $field): ?int|false {}
public function getMinimalDaysInFirstWeek(): ?int|false {}
public function setMinimalDaysInFirstWeek(int $days) {}
public function getMinimum(int $field): ?int|false {}
public static function getNow(): ?float {}
public function getRepeatedWallTimeOption(): ?int {}
public function getSkippedWallTimeOption(): ?int {}
public function getTime(): ?float|false {}
public function getTimeZone(): ?IntlTimeZone|false {}
public function getType(): ?string {}
public function getWeekendTransition(int $dayOfWeek): ?int|false {}
public function inDaylightTime(): ?bool {}
public function isEquivalentTo(IntlCalendar $other): ?bool {}
public function isLenient(): ?bool {}
public function isWeekend(?float $timestamp = NULL): ?bool {}
public function roll(int $field, $value): ?bool {}
public function isSet(int $field): ?bool {}
public function set(int $year, int $month, int $dayOfMonth, int $hour, int $minute, int $second) {}
public function setFirstDayOfWeek(int $dayOfWeek) {}
public function setLenient(bool $lenient) {}
public function setRepeatedWallTimeOption(int $option) {}
public function setSkippedWallTimeOption(int $option) {}
public function setTime(float $timestamp): ?bool {}
public function setTimeZone($timezone): ?bool {}
public function toDateTime(): ?DateTime|false {}
}
class IntlGregorianCalendar {
public function __construct($timezoneOrYear, $localeOrMonth, $day, $hour, $minute, $second) {}
public function setGregorianChange(float $timestamp): ?bool {}
public function getGregorianChange(): ?float {}
public function isLeapYear(int $year): ?bool {}
public static function createInstance($timezone = NULL, ?string $locale = NULL): ??IntlCalendar {}
public function equals(IntlCalendar $other): ?bool {}
public function fieldDifference(float $timestamp, int $field): ?int|false {}
public function add(int $field, int $value): ?bool {}
public function after(IntlCalendar $other): ?bool {}
public function before(IntlCalendar $other): ?bool {}
public function clear(?int $field = NULL) {}
public static function fromDateTime(DateTime|string $datetime, ?string $locale = NULL): ??IntlCalendar {}
public function get(int $field): ?int|false {}
public function getActualMaximum(int $field): ?int|false {}
public function getActualMinimum(int $field): ?int|false {}
public static function getAvailableLocales(): ?array {}
public function getDayOfWeekType(int $dayOfWeek): ?int|false {}
public function getErrorCode(): ?int|false {}
public function getErrorMessage(): ?string|false {}
public function getFirstDayOfWeek(): ?int|false {}
public function getGreatestMinimum(int $field): ?int|false {}
public static function getKeywordValuesForLocale(string $keyword, string $locale, bool $onlyCommon): ?IntlIterator|false {}
public function getLeastMaximum(int $field): ?int|false {}
public function getLocale(int $type): ?string|false {}
public function getMaximum(int $field): ?int|false {}
public function getMinimalDaysInFirstWeek(): ?int|false {}
public function setMinimalDaysInFirstWeek(int $days) {}
public function getMinimum(int $field): ?int|false {}
public static function getNow(): ?float {}
public function getRepeatedWallTimeOption(): ?int {}
public function getSkippedWallTimeOption(): ?int {}
public function getTime(): ?float|false {}
public function getTimeZone(): ?IntlTimeZone|false {}
public function getType(): ?string {}
public function getWeekendTransition(int $dayOfWeek): ?int|false {}
public function inDaylightTime(): ?bool {}
public function isEquivalentTo(IntlCalendar $other): ?bool {}
public function isLenient(): ?bool {}
public function isWeekend(?float $timestamp = NULL): ?bool {}
public function roll(int $field, $value): ?bool {}
public function isSet(int $field): ?bool {}
public function set(int $year, int $month, int $dayOfMonth, int $hour, int $minute, int $second) {}
public function setFirstDayOfWeek(int $dayOfWeek) {}
public function setLenient(bool $lenient) {}
public function setRepeatedWallTimeOption(int $option) {}
public function setSkippedWallTimeOption(int $option) {}
public function setTime(float $timestamp): ?bool {}
public function setTimeZone($timezone): ?bool {}
public function toDateTime(): ?DateTime|false {}
}
class Spoofchecker {
public function __construct() {}
public function isSuspicious(string $string, $errorCode = NULL): ?bool {}
public function areConfusable(string $string1, string $string2, $errorCode = NULL): ?bool {}
public function setAllowedLocales(string $locales): ?void {}
public function setChecks(int $checks): ?void {}
public function setRestrictionLevel(int $level): ?void {}
}
class IntlException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class IntlIterator {
public function current(): ?mixed {}
public function key(): ?mixed {}
public function next(): ?void {}
public function rewind(): ?void {}
public function valid(): ?bool {}
}
class IntlBreakIterator {
public static function createCharacterInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createCodePointInstance(): ?IntlCodePointBreakIterator {}
public static function createLineInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createSentenceInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createTitleInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createWordInstance(?string $locale = NULL): ??IntlBreakIterator {}
private function __construct() {}
public function current(): ?int {}
public function first(): ?int {}
public function following(int $offset): ?int {}
public function getErrorCode(): ?int {}
public function getErrorMessage(): ?string {}
public function getLocale(int $type): ?string|false {}
public function getPartsIterator(string $type = 0): ?IntlPartsIterator {}
public function getText(): ??string {}
public function isBoundary(int $offset): ?bool {}
public function last(): ?int {}
public function next(?int $offset = NULL): ?int {}
public function preceding(int $offset): ?int {}
public function previous(): ?int {}
public function setText(string $text): ??bool {}
public function getIterator(): Iterator {}
}
class IntlRuleBasedBreakIterator {
public function __construct(string $rules, bool $compiled = false) {}
public function getBinaryRules(): ?string|false {}
public function getRules(): ?string|false {}
public function getRuleStatus(): ?int {}
public function getRuleStatusVec(): ?array|false {}
public static function createCharacterInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createCodePointInstance(): ?IntlCodePointBreakIterator {}
public static function createLineInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createSentenceInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createTitleInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createWordInstance(?string $locale = NULL): ??IntlBreakIterator {}
public function current(): ?int {}
public function first(): ?int {}
public function following(int $offset): ?int {}
public function getErrorCode(): ?int {}
public function getErrorMessage(): ?string {}
public function getLocale(int $type): ?string|false {}
public function getPartsIterator(string $type = 0): ?IntlPartsIterator {}
public function getText(): ??string {}
public function isBoundary(int $offset): ?bool {}
public function last(): ?int {}
public function next(?int $offset = NULL): ?int {}
public function preceding(int $offset): ?int {}
public function previous(): ?int {}
public function setText(string $text): ??bool {}
public function getIterator(): Iterator {}
}
class IntlCodePointBreakIterator {
public function getLastCodePoint(): ?int {}
public static function createCharacterInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createCodePointInstance(): ?IntlCodePointBreakIterator {}
public static function createLineInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createSentenceInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createTitleInstance(?string $locale = NULL): ??IntlBreakIterator {}
public static function createWordInstance(?string $locale = NULL): ??IntlBreakIterator {}
public function current(): ?int {}
public function first(): ?int {}
public function following(int $offset): ?int {}
public function getErrorCode(): ?int {}
public function getErrorMessage(): ?string {}
public function getLocale(int $type): ?string|false {}
public function getPartsIterator(string $type = 0): ?IntlPartsIterator {}
public function getText(): ??string {}
public function isBoundary(int $offset): ?bool {}
public function last(): ?int {}
public function next(?int $offset = NULL): ?int {}
public function preceding(int $offset): ?int {}
public function previous(): ?int {}
public function setText(string $text): ??bool {}
public function getIterator(): Iterator {}
}
class IntlPartsIterator {
public function getBreakIterator(): ?IntlBreakIterator {}
public function getRuleStatus(): ?int {}
public function current(): ?mixed {}
public function key(): ?mixed {}
public function next(): ?void {}
public function rewind(): ?void {}
public function valid(): ?bool {}
}
class UConverter {
public function __construct(?string $destination_encoding = NULL, ?string $source_encoding = NULL) {}
public function convert(string $str, bool $reverse = false): ?string|false {}
public function fromUCallback(int $reason, array $source, int $codePoint, $error): ?array|string|int|null {}
public static function getAliases(string $name): ?array|false|null {}
public static function getAvailable(): ?array {}
public function getDestinationEncoding(): ?string|false|null {}
public function getDestinationType(): ?int|false|null {}
public function getErrorCode(): ?int {}
public function getErrorMessage(): ??string {}
public function getSourceEncoding(): ?string|false|null {}
public function getSourceType(): ?int|false|null {}
public static function getStandards(): ??array {}
public function getSubstChars(): ?string|false|null {}
public static function reasonText(int $reason): ?string {}
public function setDestinationEncoding(string $encoding): ?bool {}
public function setSourceEncoding(string $encoding): ?bool {}
public function setSubstChars(string $chars): ?bool {}
public function toUCallback(int $reason, string $source, string $codeUnits, $error): ?array|string|int|null {}
public static function transcode(string $str, string $toEncoding, string $fromEncoding, ?array $options = NULL): ?string|false {}
}
class IntlChar {
public static function hasBinaryProperty(string|int $codepoint, int $property): ??bool {}
public static function charAge(string|int $codepoint): ??array {}
public static function charDigitValue(string|int $codepoint): ??int {}
public static function charDirection(string|int $codepoint): ??int {}
public static function charFromName(string $name, int $type = 0): ??int {}
public static function charMirror(string|int $codepoint): ?string|int|null {}
public static function charName(string|int $codepoint, int $type = 0): ??string {}
public static function charType(string|int $codepoint): ??int {}
public static function chr(string|int $codepoint): ??string {}
public static function digit(string|int $codepoint, int $base = 10): ?int|false|null {}
public static function enumCharNames(string|int $start, string|int $end, callable $callback, int $type = 0): ??bool {}
public static function enumCharTypes(callable $callback): ?void {}
public static function foldCase(string|int $codepoint, int $options = 0): ?string|int|null {}
public static function forDigit(int $digit, int $base = 10): ?int {}
public static function getBidiPairedBracket(string|int $codepoint): ?string|int|null {}
public static function getBlockCode(string|int $codepoint): ??int {}
public static function getCombiningClass(string|int $codepoint): ??int {}
public static function getFC_NFKC_Closure(string|int $codepoint): ?string|false|null {}
public static function getIntPropertyMaxValue(int $property): ?int {}
public static function getIntPropertyMinValue(int $property): ?int {}
public static function getIntPropertyValue(string|int $codepoint, int $property): ??int {}
public static function getNumericValue(string|int $codepoint): ??float {}
public static function getPropertyEnum(string $alias): ?int {}
public static function getPropertyName(int $property, int $type = 1): ?string|false {}
public static function getPropertyValueEnum(int $property, string $name): ?int {}
public static function getPropertyValueName(int $property, int $value, int $type = 1): ?string|false {}
public static function getUnicodeVersion(): ?array {}
public static function isalnum(string|int $codepoint): ??bool {}
public static function isalpha(string|int $codepoint): ??bool {}
public static function isbase(string|int $codepoint): ??bool {}
public static function isblank(string|int $codepoint): ??bool {}
public static function iscntrl(string|int $codepoint): ??bool {}
public static function isdefined(string|int $codepoint): ??bool {}
public static function isdigit(string|int $codepoint): ??bool {}
public static function isgraph(string|int $codepoint): ??bool {}
public static function isIDIgnorable(string|int $codepoint): ??bool {}
public static function isIDPart(string|int $codepoint): ??bool {}
public static function isIDStart(string|int $codepoint): ??bool {}
public static function isISOControl(string|int $codepoint): ??bool {}
public static function isJavaIDPart(string|int $codepoint): ??bool {}
public static function isJavaIDStart(string|int $codepoint): ??bool {}
public static function isJavaSpaceChar(string|int $codepoint): ??bool {}
public static function islower(string|int $codepoint): ??bool {}
public static function isMirrored(string|int $codepoint): ??bool {}
public static function isprint(string|int $codepoint): ??bool {}
public static function ispunct(string|int $codepoint): ??bool {}
public static function isspace(string|int $codepoint): ??bool {}
public static function istitle(string|int $codepoint): ??bool {}
public static function isUAlphabetic(string|int $codepoint): ??bool {}
public static function isULowercase(string|int $codepoint): ??bool {}
public static function isupper(string|int $codepoint): ??bool {}
public static function isUUppercase(string|int $codepoint): ??bool {}
public static function isUWhiteSpace(string|int $codepoint): ??bool {}
public static function isWhitespace(string|int $codepoint): ??bool {}
public static function isxdigit(string|int $codepoint): ??bool {}
public static function ord(string|int $character): ??int {}
public static function tolower(string|int $codepoint): ?string|int|null {}
public static function totitle(string|int $codepoint): ?string|int|null {}
public static function toupper(string|int $codepoint): ?string|int|null {}
}
class MessagePack {
public function __construct($opt) {}
public function setOption($option, $value) {}
public function pack($value) {}
public function unpack($str, $object) {}
public function unpacker() {}
}
class MessagePackUnpacker {
public function __construct($opt) {}
public function __destruct() {}
public function setOption($option, $value) {}
public function feed($str) {}
public function execute($str, $offset) {}
public function data($object) {}
public function reset() {}
}
final class mysqli_sql_exception {
public function getSqlState(): string {}
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
final class mysqli_driver {
}
class mysqli {
public function __construct(?string $hostname = NULL, ?string $username = NULL, ?string $password = NULL, ?string $database = NULL, ?int $port = NULL, ?string $socket = NULL) {}
public function autocommit(bool $enable): ?bool {}
public function begin_transaction(int $flags = 0, ?string $name = NULL): ?bool {}
public function change_user(string $username, string $password, ?string $database): ?bool {}
public function character_set_name(): ?string {}
public function close() {}
public function commit(int $flags = 0, ?string $name = NULL): ?bool {}
public function connect(?string $hostname = NULL, ?string $username = NULL, ?string $password = NULL, ?string $database = NULL, ?int $port = NULL, ?string $socket = NULL): ?bool {}
public function dump_debug_info(): ?bool {}
public function debug(string $options) {}
public function get_charset(): ??object {}
public function execute_query(string $query, ?array $params = NULL): mysqli_result|bool {}
public function get_client_info(): ?string {}
public function get_connection_stats(): ?array {}
public function get_server_info(): ?string {}
public function get_warnings(): ?mysqli_warning|false {}
public function init() {}
public function kill(int $process_id): ?bool {}
public function multi_query(string $query): ?bool {}
public function more_results(): ?bool {}
public function next_result(): ?bool {}
public function ping(): ?bool {}
public static function poll(?array $read, ?array $error, array $reject, int $seconds, int $microseconds = 0): ?int|false {}
public function prepare(string $query): ?mysqli_stmt|false {}
public function query(string $query, int $result_mode = 0): ?mysqli_result|bool {}
public function real_connect(?string $hostname = NULL, ?string $username = NULL, ?string $password = NULL, ?string $database = NULL, ?int $port = NULL, ?string $socket = NULL, int $flags = 0): ?bool {}
public function real_escape_string(string $string): ?string {}
public function reap_async_query(): ?mysqli_result|bool {}
public function escape_string(string $string): ?string {}
public function real_query(string $query): ?bool {}
public function release_savepoint(string $name): ?bool {}
public function rollback(int $flags = 0, ?string $name = NULL): ?bool {}
public function savepoint(string $name): ?bool {}
public function select_db(string $database): ?bool {}
public function set_charset(string $charset): ?bool {}
public function options(int $option, $value): ?bool {}
public function set_opt(int $option, $value): ?bool {}
public function ssl_set(?string $key, ?string $certificate, ?string $ca_certificate, ?string $ca_path, ?string $cipher_algos) {}
public function stat(): ?string|false {}
public function stmt_init(): ?mysqli_stmt|false {}
public function store_result(int $mode = 0): ?mysqli_result|false {}
public function thread_safe(): ?bool {}
public function use_result(): ?mysqli_result|false {}
public function refresh(int $flags): ?bool {}
}
final class mysqli_warning {
private function __construct() {}
public function next(): bool {}
}
class mysqli_result {
public function __construct(mysqli $mysql, int $result_mode = 0) {}
public function close(): ?void {}
public function free(): ?void {}
public function data_seek(int $offset): ?bool {}
public function fetch_field(): ?object|false {}
public function fetch_fields(): ?array {}
public function fetch_field_direct(int $index): ?object|false {}
public function fetch_all(int $mode = 2): ?array {}
public function fetch_array(int $mode = 3): ?array|false|null {}
public function fetch_assoc(): ?array|false|null {}
public function fetch_object(string $class = 'stdClass', array $constructor_args = array (
)): ?object|false|null {}
public function fetch_row(): ?array|false|null {}
public function fetch_column(int $column = 0): string|int|float|false|null {}
public function field_seek(int $index): ?bool {}
public function free_result(): ?void {}
public function getIterator(): Iterator {}
}
class mysqli_stmt {
public function __construct(mysqli $mysql, ?string $query = NULL) {}
public function attr_get(int $attribute): ?int {}
public function attr_set(int $attribute, int $value): ?bool {}
public function bind_param(string $types, mixed $vars): ?bool {}
public function bind_result(mixed $vars): ?bool {}
public function close() {}
public function data_seek(int $offset): ?void {}
public function execute(?array $params = NULL): ?bool {}
public function fetch(): ??bool {}
public function get_warnings(): ?mysqli_warning|false {}
public function result_metadata(): ?mysqli_result|false {}
public function more_results(): ?bool {}
public function next_result(): ?bool {}
public function num_rows(): ?string|int {}
public function send_long_data(int $param_num, string $data): ?bool {}
public function free_result(): ?void {}
public function reset(): ?bool {}
public function prepare(string $query): ?bool {}
public function store_result(): ?bool {}
public function get_result(): ?mysqli_result|false {}
}
namespace PgSql {
final class Connection {
}
}
namespace PgSql {
final class Result {
}
}
namespace PgSql {
final class Lob {
}
}
class PharException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class Phar {
public function __construct(string $filename, int $flags = 12288, ?string $alias = NULL) {}
public function __destruct() {}
public function addEmptyDir(string $directory): ?void {}
public function addFile(string $filename, ?string $localName = NULL): ?void {}
public function addFromString(string $localName, string $contents): ?void {}
public function buildFromDirectory(string $directory, string $pattern = ''): ?array {}
public function buildFromIterator(Traversable $iterator, ?string $baseDirectory = NULL): ?array {}
public function compressFiles(int $compression): ?void {}
public function decompressFiles() {}
public function compress(int $compression, ?string $extension = NULL): ??Phar {}
public function decompress(?string $extension = NULL): ??Phar {}
public function convertToExecutable(?int $format = NULL, ?int $compression = NULL, ?string $extension = NULL): ??Phar {}
public function convertToData(?int $format = NULL, ?int $compression = NULL, ?string $extension = NULL): ??PharData {}
public function copy(string $from, string $to) {}
public function count(int $mode = 0): ?int {}
public function delete(string $localName) {}
public function delMetadata() {}
public function extractTo(string $directory, array|string|null $files = NULL, bool $overwrite = false): ?bool {}
public function getAlias(): ??string {}
public function getPath(): ?string {}
public function getMetadata(array $unserializeOptions = array (
)): ?mixed {}
public function getModified(): ?bool {}
public function getSignature(): ?array|false {}
public function getStub(): ?string {}
public function getVersion(): ?string {}
public function hasMetadata(): ?bool {}
public function isBuffering(): ?bool {}
public function isCompressed(): ?int|false {}
public function isFileFormat(int $format): ?bool {}
public function isWritable(): ?bool {}
public function offsetExists($localName): ?bool {}
public function offsetGet($localName): ?SplFileInfo {}
public function offsetSet($localName, $value): ?void {}
public function offsetUnset($localName): ?void {}
public function setAlias(string $alias): ?bool {}
public function setDefaultStub(?string $index = NULL, ?string $webIndex = NULL): ?bool {}
public function setMetadata(mixed $metadata): ?void {}
public function setSignatureAlgorithm(int $algo, ?string $privateKey = NULL): ?void {}
public function setStub($stub, int $length) {}
public function startBuffering(): ?void {}
public function stopBuffering(): ?void {}
final public static function apiVersion(): string {}
final public static function canCompress(int $compression = 0): bool {}
final public static function canWrite(): bool {}
final public static function createDefaultStub(?string $index = NULL, ?string $webIndex = NULL): string {}
final public static function getSupportedCompression(): array {}
final public static function getSupportedSignatures(): array {}
final public static function interceptFileFuncs(): void {}
final public static function isValidPharFilename(string $filename, bool $executable = true): bool {}
final public static function loadPhar(string $filename, ?string $alias = NULL): bool {}
final public static function mapPhar(?string $alias = NULL, int $offset = 0): bool {}
final public static function running(bool $returnPhar = true): string {}
final public static function mount(string $pharPath, string $externalPath): void {}
final public static function mungServer(array $variables): void {}
final public static function unlinkArchive(string $filename): bool {}
final public static function webPhar(?string $alias = NULL, ?string $index = NULL, ?string $fileNotFoundScript = NULL, array $mimeTypes = array (
), ?callable $rewrite = NULL): void {}
public function hasChildren(bool $allowLinks = false): ?bool {}
public function getChildren(): ?RecursiveDirectoryIterator {}
public function getSubPath(): ?string {}
public function getSubPathname(): ?string {}
public function rewind(): ?void {}
public function key(): ?string {}
public function current(): ?SplFileInfo|FilesystemIterator|string {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function isDot(): ?bool {}
public function valid(): ?bool {}
public function next(): ?void {}
public function seek(int $offset): ?void {}
public function __toString(): string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class PharData {
public function __construct(string $filename, int $flags = 12288, ?string $alias = NULL, int $format = 0) {}
public function __destruct() {}
public function addEmptyDir(string $directory): ?void {}
public function addFile(string $filename, ?string $localName = NULL): ?void {}
public function addFromString(string $localName, string $contents): ?void {}
public function buildFromDirectory(string $directory, string $pattern = ''): ?array {}
public function buildFromIterator(Traversable $iterator, ?string $baseDirectory = NULL): ?array {}
public function compressFiles(int $compression): ?void {}
public function decompressFiles() {}
public function compress(int $compression, ?string $extension = NULL): ??PharData {}
public function decompress(?string $extension = NULL): ??PharData {}
public function convertToExecutable(?int $format = NULL, ?int $compression = NULL, ?string $extension = NULL): ??Phar {}
public function convertToData(?int $format = NULL, ?int $compression = NULL, ?string $extension = NULL): ??PharData {}
public function copy(string $from, string $to) {}
public function count(int $mode = 0): ?int {}
public function delete(string $localName) {}
public function delMetadata() {}
public function extractTo(string $directory, array|string|null $files = NULL, bool $overwrite = false): ?bool {}
public function getAlias(): ??string {}
public function getPath(): ?string {}
public function getMetadata(array $unserializeOptions = array (
)): ?mixed {}
public function getModified(): ?bool {}
public function getSignature(): ?array|false {}
public function getStub(): ?string {}
public function getVersion(): ?string {}
public function hasMetadata(): ?bool {}
public function isBuffering(): ?bool {}
public function isCompressed(): ?int|false {}
public function isFileFormat(int $format): ?bool {}
public function isWritable(): ?bool {}
public function offsetExists($localName): ?bool {}
public function offsetGet($localName): ?SplFileInfo {}
public function offsetSet($localName, $value): ?void {}
public function offsetUnset($localName): ?void {}
public function setAlias(string $alias): ?bool {}
public function setDefaultStub(?string $index = NULL, ?string $webIndex = NULL): ?bool {}
public function setMetadata(mixed $metadata): ?void {}
public function setSignatureAlgorithm(int $algo, ?string $privateKey = NULL): ?void {}
public function setStub($stub, int $length) {}
public function startBuffering(): ?void {}
public function stopBuffering(): ?void {}
final public static function apiVersion(): string {}
final public static function canCompress(int $compression = 0): bool {}
final public static function canWrite(): bool {}
final public static function createDefaultStub(?string $index = NULL, ?string $webIndex = NULL): string {}
final public static function getSupportedCompression(): array {}
final public static function getSupportedSignatures(): array {}
final public static function interceptFileFuncs(): void {}
final public static function isValidPharFilename(string $filename, bool $executable = true): bool {}
final public static function loadPhar(string $filename, ?string $alias = NULL): bool {}
final public static function mapPhar(?string $alias = NULL, int $offset = 0): bool {}
final public static function running(bool $returnPhar = true): string {}
final public static function mount(string $pharPath, string $externalPath): void {}
final public static function mungServer(array $variables): void {}
final public static function unlinkArchive(string $filename): bool {}
final public static function webPhar(?string $alias = NULL, ?string $index = NULL, ?string $fileNotFoundScript = NULL, array $mimeTypes = array (
), ?callable $rewrite = NULL): void {}
public function hasChildren(bool $allowLinks = false): ?bool {}
public function getChildren(): ?RecursiveDirectoryIterator {}
public function getSubPath(): ?string {}
public function getSubPathname(): ?string {}
public function rewind(): ?void {}
public function key(): ?string {}
public function current(): ?SplFileInfo|FilesystemIterator|string {}
public function getFlags(): ?int {}
public function setFlags(int $flags): ?void {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function isDot(): ?bool {}
public function valid(): ?bool {}
public function next(): ?void {}
public function seek(int $offset): ?void {}
public function __toString(): string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class PharFileInfo {
public function __construct(string $filename) {}
public function __destruct() {}
public function chmod(int $perms): ?void {}
public function compress(int $compression) {}
public function decompress() {}
public function delMetadata() {}
public function getCompressedSize(): ?int {}
public function getCRC32(): ?int {}
public function getContent(): ?string {}
public function getMetadata(array $unserializeOptions = array (
)): ?mixed {}
public function getPharFlags(): ?int {}
public function hasMetadata(): ?bool {}
public function isCompressed(?int $compression = NULL): ?bool {}
public function isCRCChecked(): ?bool {}
public function setMetadata(mixed $metadata): ?void {}
public function getPath(): ?string {}
public function getFilename(): ?string {}
public function getExtension(): ?string {}
public function getBasename(string $suffix = ''): ?string {}
public function getPathname(): ?string {}
public function getPerms(): ?int|false {}
public function getInode(): ?int|false {}
public function getSize(): ?int|false {}
public function getOwner(): ?int|false {}
public function getGroup(): ?int|false {}
public function getATime(): ?int|false {}
public function getMTime(): ?int|false {}
public function getCTime(): ?int|false {}
public function getType(): ?string|false {}
public function isWritable(): ?bool {}
public function isReadable(): ?bool {}
public function isExecutable(): ?bool {}
public function isFile(): ?bool {}
public function isDir(): ?bool {}
public function isLink(): ?bool {}
public function getLinkTarget(): ?string|false {}
public function getRealPath(): ?string|false {}
public function getFileInfo(?string $class = NULL): ?SplFileInfo {}
public function getPathInfo(?string $class = NULL): ??SplFileInfo {}
public function openFile(string $mode = 'r', bool $useIncludePath = false, $context = NULL): ?SplFileObject {}
public function setFileClass(string $class = 'SplFileObject'): ?void {}
public function setInfoClass(string $class = 'SplFileInfo'): ?void {}
public function __toString(): string {}
public function __debugInfo(): ?array {}
final public function _bad_state_ex(): ?void {}
}
class Redis {
public function __construct() {}
public function __destruct() {}
public function _prefix($key) {}
public function _serialize($value) {}
public function _unserialize($value) {}
public function _pack($value) {}
public function _unpack($value) {}
public function _compress($value) {}
public function _uncompress($value) {}
public function acl($subcmd, $args) {}
public function append($key, $value) {}
public function auth($auth) {}
public function bgSave() {}
public function bgrewriteaof() {}
public function bitcount($key) {}
public function bitop($operation, $ret_key, $key, $other_keys) {}
public function bitpos($key, $bit, $start, $end) {}
public function blPop($key, $timeout_or_key, $extra_args) {}
public function brPop($key, $timeout_or_key, $extra_args) {}
public function brpoplpush($src, $dst, $timeout) {}
public function bzPopMax($key, $timeout_or_key, $extra_args) {}
public function bzPopMin($key, $timeout_or_key, $extra_args) {}
public function clearLastError() {}
public function client($cmd, $args) {}
public function close() {}
public function command($args) {}
public function config($cmd, $key, $value) {}
public function connect($host, $port, $timeout, $retry_interval) {}
public function dbSize() {}
public function debug($key) {}
public function decr($key) {}
public function decrBy($key, $value) {}
public function del($key, $other_keys) {}
public function discard() {}
public function dump($key) {}
public function echo($msg) {}
public function eval($script, $args, $num_keys) {}
public function evalsha($script_sha, $args, $num_keys) {}
public function exec() {}
public function exists($key, $other_keys) {}
public function expire($key, $timeout) {}
public function expireAt($key, $timestamp) {}
public function flushAll($async) {}
public function flushDB($async) {}
public function geoadd($key, $lng, $lat, $member, $other_triples) {}
public function geodist($key, $src, $dst, $unit) {}
public function geohash($key, $member, $other_members) {}
public function geopos($key, $member, $other_members) {}
public function georadius($key, $lng, $lan, $radius, $unit, array $opts) {}
public function georadius_ro($key, $lng, $lan, $radius, $unit, array $opts) {}
public function georadiusbymember($key, $member, $radius, $unit, array $opts) {}
public function georadiusbymember_ro($key, $member, $radius, $unit, array $opts) {}
public function get($key) {}
public function getAuth() {}
public function getBit($key, $offset) {}
public function getDBNum() {}
public function getHost() {}
public function getLastError() {}
public function getMode() {}
public function getOption($option) {}
public function getPersistentID() {}
public function getPort() {}
public function getRange($key, $start, $end) {}
public function getReadTimeout() {}
public function getSet($key, $value) {}
public function getTimeout() {}
public function hDel($key, $member, $other_members) {}
public function hExists($key, $member) {}
public function hGet($key, $member) {}
public function hGetAll($key) {}
public function hIncrBy($key, $member, $value) {}
public function hIncrByFloat($key, $member, $value) {}
public function hKeys($key) {}
public function hLen($key) {}
public function hMget($key, array $keys) {}
public function hMset($key, array $pairs) {}
public function hSet($key, $member, $value) {}
public function hSetNx($key, $member, $value) {}
public function hStrLen($key, $member) {}
public function hVals($key) {}
public function hscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function incr($key) {}
public function incrBy($key, $value) {}
public function incrByFloat($key, $value) {}
public function info($option) {}
public function isConnected() {}
public function keys($pattern) {}
public function lInsert($key, $position, $pivot, $value) {}
public function lLen($key) {}
public function lPop($key) {}
public function lPush($key, $value) {}
public function lPushx($key, $value) {}
public function lSet($key, $index, $value) {}
public function lastSave() {}
public function lindex($key, $index) {}
public function lrange($key, $start, $end) {}
public function lrem($key, $value, $count) {}
public function ltrim($key, $start, $stop) {}
public function mget(array $keys) {}
public function migrate($host, $port, $key, $db, $timeout, $copy, $replace) {}
public function move($key, $dbindex) {}
public function mset(array $pairs) {}
public function msetnx(array $pairs) {}
public function multi($mode) {}
public function object($field, $key) {}
public function pconnect($host, $port, $timeout) {}
public function persist($key) {}
public function pexpire($key, $timestamp) {}
public function pexpireAt($key, $timestamp) {}
public function pfadd($key, array $elements) {}
public function pfcount($key) {}
public function pfmerge($dstkey, array $keys) {}
public function ping() {}
public function pipeline() {}
public function psetex($key, $expire, $value) {}
public function psubscribe(array $patterns, $callback) {}
public function pttl($key) {}
public function publish($channel, $message) {}
public function pubsub($cmd, $args) {}
public function punsubscribe($pattern, $other_patterns) {}
public function rPop($key) {}
public function rPush($key, $value) {}
public function rPushx($key, $value) {}
public function randomKey() {}
public function rawcommand($cmd, $args) {}
public function rename($key, $newkey) {}
public function renameNx($key, $newkey) {}
public function restore($ttl, $key, $value) {}
public function role() {}
public function rpoplpush($src, $dst) {}
public function sAdd($key, $value) {}
public function sAddArray($key, array $options) {}
public function sDiff($key, $other_keys) {}
public function sDiffStore($dst, $key, $other_keys) {}
public function sInter($key, $other_keys) {}
public function sInterStore($dst, $key, $other_keys) {}
public function sMembers($key) {}
public function sMisMember($key, $member, $other_members) {}
public function sMove($src, $dst, $value) {}
public function sPop($key) {}
public function sRandMember($key, $count) {}
public function sUnion($key, $other_keys) {}
public function sUnionStore($dst, $key, $other_keys) {}
public function save() {}
public function scan($i_iterator, $str_pattern, $i_count) {}
public function scard($key) {}
public function script($cmd, $args) {}
public function select($dbindex) {}
public function set($key, $value, $opts) {}
public function setBit($key, $offset, $value) {}
public function setOption($option, $value) {}
public function setRange($key, $offset, $value) {}
public function setex($key, $expire, $value) {}
public function setnx($key, $value) {}
public function sismember($key, $value) {}
public function slaveof($host, $port) {}
public function slowlog($arg, $option) {}
public function sort($key, array $options) {}
public function sortAsc($key, $pattern, $get, $start, $end, $getList) {}
public function sortAscAlpha($key, $pattern, $get, $start, $end, $getList) {}
public function sortDesc($key, $pattern, $get, $start, $end, $getList) {}
public function sortDescAlpha($key, $pattern, $get, $start, $end, $getList) {}
public function srem($key, $member, $other_members) {}
public function sscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function strlen($key) {}
public function subscribe(array $channels, $callback) {}
public function swapdb($srcdb, $dstdb) {}
public function time() {}
public function ttl($key) {}
public function type($key) {}
public function unlink($key, $other_keys) {}
public function unsubscribe($channel, $other_channels) {}
public function unwatch() {}
public function wait($numslaves, $timeout) {}
public function watch($key, $other_keys) {}
public function xack($str_key, $str_group, array $arr_ids) {}
public function xadd($str_key, $str_id, array $arr_fields, $i_maxlen, $boo_approximate) {}
public function xclaim($str_key, $str_group, $str_consumer, $i_min_idle, array $arr_ids, array $arr_opts) {}
public function xdel($str_key, array $arr_ids) {}
public function xgroup($str_operation, $str_key, $str_arg1, $str_arg2, $str_arg3) {}
public function xinfo($str_cmd, $str_key, $str_group) {}
public function xlen($key) {}
public function xpending($str_key, $str_group, $str_start, $str_end, $i_count, $str_consumer) {}
public function xrange($str_key, $str_start, $str_end, $i_count) {}
public function xread(array $arr_streams, $i_count, $i_block) {}
public function xreadgroup($str_group, $str_consumer, array $arr_streams, $i_count, $i_block) {}
public function xrevrange($str_key, $str_start, $str_end, $i_count) {}
public function xtrim($str_key, $i_maxlen, $boo_approximate) {}
public function zAdd($key, $score, $value, $extra_args) {}
public function zCard($key) {}
public function zCount($key, $min, $max) {}
public function zIncrBy($key, $value, $member) {}
public function zLexCount($key, $min, $max) {}
public function zPopMax($key) {}
public function zPopMin($key) {}
public function zRange($key, $start, $end, $scores) {}
public function zRangeByLex($key, $min, $max, $offset, $limit) {}
public function zRangeByScore($key, $start, $end, array $options) {}
public function zRank($key, $member) {}
public function zRem($key, $member, $other_members) {}
public function zRemRangeByLex($key, $min, $max) {}
public function zRemRangeByRank($key, $start, $end) {}
public function zRemRangeByScore($key, $min, $max) {}
public function zRevRange($key, $start, $end, $scores) {}
public function zRevRangeByLex($key, $min, $max, $offset, $limit) {}
public function zRevRangeByScore($key, $start, $end, array $options) {}
public function zRevRank($key, $member) {}
public function zScore($key, $member) {}
public function zinterstore($key, array $keys, ?array $weights, $aggregate) {}
public function zscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function zunionstore($key, array $keys, ?array $weights, $aggregate) {}
public function delete($key, $other_keys) {}
public function evaluate($script, $args, $num_keys) {}
public function evaluateSha($script_sha, $args, $num_keys) {}
public function getKeys($pattern) {}
public function getMultiple(array $keys) {}
public function lGet($key, $index) {}
public function lGetRange($key, $start, $end) {}
public function lRemove($key, $value, $count) {}
public function lSize($key) {}
public function listTrim($key, $start, $stop) {}
public function open($host, $port, $timeout, $retry_interval) {}
public function popen($host, $port, $timeout) {}
public function renameKey($key, $newkey) {}
public function sContains($key, $value) {}
public function sGetMembers($key) {}
public function sRemove($key, $member, $other_members) {}
public function sSize($key) {}
public function sendEcho($msg) {}
public function setTimeout($key, $timeout) {}
public function substr($key, $start, $end) {}
public function zDelete($key, $member, $other_members) {}
public function zDeleteRangeByRank($key, $min, $max) {}
public function zDeleteRangeByScore($key, $min, $max) {}
public function zInter($key, array $keys, ?array $weights, $aggregate) {}
public function zRemove($key, $member, $other_members) {}
public function zRemoveRangeByScore($key, $min, $max) {}
public function zReverseRange($key, $start, $end, $scores) {}
public function zSize($key) {}
public function zUnion($key, array $keys, ?array $weights, $aggregate) {}
}
class RedisArray {
public function __call($function_name, $arguments) {}
public function __construct($name_or_hosts, array $options) {}
public function _continuum() {}
public function _distributor() {}
public function _function() {}
public function _hosts() {}
public function _instance($host) {}
public function _rehash($callable) {}
public function _target($key) {}
public function bgsave() {}
public function del($keys) {}
public function discard() {}
public function exec() {}
public function flushall($async) {}
public function flushdb($async) {}
public function getOption($opt) {}
public function hscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function info() {}
public function keys($pattern) {}
public function mget($keys) {}
public function mset($pairs) {}
public function multi($host, $mode) {}
public function ping() {}
public function save() {}
public function scan($iterator, $node, $pattern, $count) {}
public function select($index) {}
public function setOption($opt, $value) {}
public function sscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function unlink() {}
public function unwatch() {}
public function zscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function delete($keys) {}
public function getMultiple($keys) {}
}
class RedisCluster {
public function __construct($name, array $seeds, $timeout, $read_timeout, $persistent, $auth) {}
public function _masters() {}
public function _prefix($key) {}
public function _redir() {}
public function _serialize($value) {}
public function _unserialize($value) {}
public function _compress($value) {}
public function _uncompress($value) {}
public function _pack($value) {}
public function _unpack($value) {}
public function acl($key_or_address, $subcmd, $args) {}
public function append($key, $value) {}
public function bgrewriteaof($key_or_address) {}
public function bgsave($key_or_address) {}
public function bitcount($key) {}
public function bitop($operation, $ret_key, $key, $other_keys) {}
public function bitpos($key, $bit, $start, $end) {}
public function blpop($key, $timeout_or_key, $extra_args) {}
public function brpop($key, $timeout_or_key, $extra_args) {}
public function brpoplpush($src, $dst, $timeout) {}
public function clearlasterror() {}
public function bzpopmax($key, $timeout_or_key, $extra_args) {}
public function bzpopmin($key, $timeout_or_key, $extra_args) {}
public function client($key_or_address, $arg, $other_args) {}
public function close() {}
public function cluster($key_or_address, $arg, $other_args) {}
public function command($args) {}
public function config($key_or_address, $arg, $other_args) {}
public function dbsize($key_or_address) {}
public function decr($key) {}
public function decrby($key, $value) {}
public function del($key, $other_keys) {}
public function discard() {}
public function dump($key) {}
public function echo($msg) {}
public function eval($script, $args, $num_keys) {}
public function evalsha($script_sha, $args, $num_keys) {}
public function exec() {}
public function exists($key) {}
public function expire($key, $timeout) {}
public function expireat($key, $timestamp) {}
public function flushall($key_or_address, $async) {}
public function flushdb($key_or_address, $async) {}
public function geoadd($key, $lng, $lat, $member, $other_triples) {}
public function geodist($key, $src, $dst, $unit) {}
public function geohash($key, $member, $other_members) {}
public function geopos($key, $member, $other_members) {}
public function georadius($key, $lng, $lan, $radius, $unit, array $opts) {}
public function georadius_ro($key, $lng, $lan, $radius, $unit, array $opts) {}
public function georadiusbymember($key, $member, $radius, $unit, array $opts) {}
public function georadiusbymember_ro($key, $member, $radius, $unit, array $opts) {}
public function get($key) {}
public function getbit($key, $offset) {}
public function getlasterror() {}
public function getmode() {}
public function getoption($option) {}
public function getrange($key, $start, $end) {}
public function getset($key, $value) {}
public function hdel($key, $member, $other_members) {}
public function hexists($key, $member) {}
public function hget($key, $member) {}
public function hgetall($key) {}
public function hincrby($key, $member, $value) {}
public function hincrbyfloat($key, $member, $value) {}
public function hkeys($key) {}
public function hlen($key) {}
public function hmget($key, array $keys) {}
public function hmset($key, array $pairs) {}
public function hscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function hset($key, $member, $value) {}
public function hsetnx($key, $member, $value) {}
public function hstrlen($key, $member) {}
public function hvals($key) {}
public function incr($key) {}
public function incrby($key, $value) {}
public function incrbyfloat($key, $value) {}
public function info($key_or_address, $option) {}
public function keys($pattern) {}
public function lastsave($key_or_address) {}
public function lget($key, $index) {}
public function lindex($key, $index) {}
public function linsert($key, $position, $pivot, $value) {}
public function llen($key) {}
public function lpop($key) {}
public function lpush($key, $value) {}
public function lpushx($key, $value) {}
public function lrange($key, $start, $end) {}
public function lrem($key, $value) {}
public function lset($key, $index, $value) {}
public function ltrim($key, $start, $stop) {}
public function mget(array $keys) {}
public function mset(array $pairs) {}
public function msetnx(array $pairs) {}
public function multi() {}
public function object($field, $key) {}
public function persist($key) {}
public function pexpire($key, $timestamp) {}
public function pexpireat($key, $timestamp) {}
public function pfadd($key, array $elements) {}
public function pfcount($key) {}
public function pfmerge($dstkey, array $keys) {}
public function ping($key_or_address) {}
public function psetex($key, $expire, $value) {}
public function psubscribe(array $patterns, $callback) {}
public function pttl($key) {}
public function publish($channel, $message) {}
public function pubsub($key_or_address, $arg, $other_args) {}
public function punsubscribe($pattern, $other_patterns) {}
public function randomkey($key_or_address) {}
public function rawcommand($cmd, $args) {}
public function rename($key, $newkey) {}
public function renamenx($key, $newkey) {}
public function restore($ttl, $key, $value) {}
public function role() {}
public function rpop($key) {}
public function rpoplpush($src, $dst) {}
public function rpush($key, $value) {}
public function rpushx($key, $value) {}
public function sadd($key, $value) {}
public function saddarray($key, array $options) {}
public function save($key_or_address) {}
public function scan($i_iterator, $str_node, $str_pattern, $i_count) {}
public function scard($key) {}
public function script($key_or_address, $arg, $other_args) {}
public function sdiff($key, $other_keys) {}
public function sdiffstore($dst, $key, $other_keys) {}
public function set($key, $value, $opts) {}
public function setbit($key, $offset, $value) {}
public function setex($key, $expire, $value) {}
public function setnx($key, $value) {}
public function setoption($option, $value) {}
public function setrange($key, $offset, $value) {}
public function sinter($key, $other_keys) {}
public function sinterstore($dst, $key, $other_keys) {}
public function sismember($key, $value) {}
public function slowlog($key_or_address, $arg, $other_args) {}
public function smembers($key) {}
public function smove($src, $dst, $value) {}
public function sort($key, array $options) {}
public function spop($key) {}
public function srandmember($key, $count) {}
public function srem($key, $value) {}
public function sscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function strlen($key) {}
public function subscribe(array $channels, $callback) {}
public function sunion($key, $other_keys) {}
public function sunionstore($dst, $key, $other_keys) {}
public function time() {}
public function ttl($key) {}
public function type($key) {}
public function unsubscribe($channel, $other_channels) {}
public function unlink($key, $other_keys) {}
public function unwatch() {}
public function watch($key, $other_keys) {}
public function xack($str_key, $str_group, array $arr_ids) {}
public function xadd($str_key, $str_id, array $arr_fields, $i_maxlen, $boo_approximate) {}
public function xclaim($str_key, $str_group, $str_consumer, $i_min_idle, array $arr_ids, array $arr_opts) {}
public function xdel($str_key, array $arr_ids) {}
public function xgroup($str_operation, $str_key, $str_arg1, $str_arg2, $str_arg3) {}
public function xinfo($str_cmd, $str_key, $str_group) {}
public function xlen($key) {}
public function xpending($str_key, $str_group, $str_start, $str_end, $i_count, $str_consumer) {}
public function xrange($str_key, $str_start, $str_end, $i_count) {}
public function xread(array $arr_streams, $i_count, $i_block) {}
public function xreadgroup($str_group, $str_consumer, array $arr_streams, $i_count, $i_block) {}
public function xrevrange($str_key, $str_start, $str_end, $i_count) {}
public function xtrim($str_key, $i_maxlen, $boo_approximate) {}
public function zadd($key, $score, $value, $extra_args) {}
public function zcard($key) {}
public function zcount($key, $min, $max) {}
public function zincrby($key, $value, $member) {}
public function zinterstore($key, array $keys, ?array $weights, $aggregate) {}
public function zlexcount($key, $min, $max) {}
public function zpopmax($key) {}
public function zpopmin($key) {}
public function zrange($key, $start, $end, $scores) {}
public function zrangebylex($key, $min, $max, $offset, $limit) {}
public function zrangebyscore($key, $start, $end, array $options) {}
public function zrank($key, $member) {}
public function zrem($key, $member, $other_members) {}
public function zremrangebylex($key, $min, $max) {}
public function zremrangebyrank($key, $min, $max) {}
public function zremrangebyscore($key, $min, $max) {}
public function zrevrange($key, $start, $end, $scores) {}
public function zrevrangebylex($key, $min, $max, $offset, $limit) {}
public function zrevrangebyscore($key, $start, $end, array $options) {}
public function zrevrank($key, $member) {}
public function zscan($str_key, $i_iterator, $str_pattern, $i_count) {}
public function zscore($key, $member) {}
public function zunionstore($key, array $keys, ?array $weights, $aggregate) {}
}
class RedisSentinel {
public function __construct($host, $port, $timeout, $persistent, $retry_interval, $read_timeout) {}
public function ckquorum($value) {}
public function failover($value) {}
public function flushconfig() {}
public function getMasterAddrByName($value) {}
public function master($value) {}
public function masters() {}
public function ping() {}
public function reset($value) {}
public function sentinels($value) {}
public function slaves($value) {}
}
class RedisException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
class RedisClusterException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
final class Shmop {
}
class SimpleXMLElement {
public function xpath(string $expression): ?array|false|null {}
public function registerXPathNamespace(string $prefix, string $namespace): ?bool {}
public function asXML(?string $filename = NULL): ?string|bool {}
public function saveXML(?string $filename = NULL): ?string|bool {}
public function getNamespaces(bool $recursive = false): ?array {}
public function getDocNamespaces(bool $recursive = false, bool $fromRoot = true): ?array|false {}
public function children(?string $namespaceOrPrefix = NULL, bool $isPrefix = false): ??SimpleXMLElement {}
public function attributes(?string $namespaceOrPrefix = NULL, bool $isPrefix = false): ??SimpleXMLElement {}
public function __construct(string $data, int $options = 0, bool $dataIsURL = false, string $namespaceOrPrefix = '', bool $isPrefix = false) {}
public function addChild(string $qualifiedName, ?string $value = NULL, ?string $namespace = NULL): ??SimpleXMLElement {}
public function addAttribute(string $qualifiedName, string $value, ?string $namespace = NULL): ?void {}
public function getName(): ?string {}
public function __toString(): string {}
public function count(): ?int {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function current(): ?SimpleXMLElement {}
public function key(): ?string {}
public function next(): ?void {}
public function hasChildren(): ?bool {}
public function getChildren(): ??SimpleXMLElement {}
}
class SimpleXMLIterator {
public function xpath(string $expression): ?array|false|null {}
public function registerXPathNamespace(string $prefix, string $namespace): ?bool {}
public function asXML(?string $filename = NULL): ?string|bool {}
public function saveXML(?string $filename = NULL): ?string|bool {}
public function getNamespaces(bool $recursive = false): ?array {}
public function getDocNamespaces(bool $recursive = false, bool $fromRoot = true): ?array|false {}
public function children(?string $namespaceOrPrefix = NULL, bool $isPrefix = false): ??SimpleXMLElement {}
public function attributes(?string $namespaceOrPrefix = NULL, bool $isPrefix = false): ??SimpleXMLElement {}
public function __construct(string $data, int $options = 0, bool $dataIsURL = false, string $namespaceOrPrefix = '', bool $isPrefix = false) {}
public function addChild(string $qualifiedName, ?string $value = NULL, ?string $namespace = NULL): ??SimpleXMLElement {}
public function addAttribute(string $qualifiedName, string $value, ?string $namespace = NULL): ?void {}
public function getName(): ?string {}
public function __toString(): string {}
public function count(): ?int {}
public function rewind(): ?void {}
public function valid(): ?bool {}
public function current(): ?SimpleXMLElement {}
public function key(): ?string {}
public function next(): ?void {}
public function hasChildren(): ?bool {}
public function getChildren(): ??SimpleXMLElement {}
}
final class Socket {
}
final class AddressInfo {
}
final class SysvMessageQueue {
}
final class SysvSemaphore {
}
final class SysvSharedMemory {
}
class tidy {
public function __construct(?string $filename = NULL, array|string|null $config = NULL, ?string $encoding = NULL, bool $useIncludePath = false) {}
public function getOpt(string $option): ?string|int|bool {}
public function cleanRepair(): ?bool {}
public function parseFile(string $filename, array|string|null $config = NULL, ?string $encoding = NULL, bool $useIncludePath = false): ?bool {}
public function parseString(string $string, array|string|null $config = NULL, ?string $encoding = NULL): ?bool {}
public static function repairString(string $string, array|string|null $config = NULL, ?string $encoding = NULL): ?string|false {}
public static function repairFile(string $filename, array|string|null $config = NULL, ?string $encoding = NULL, bool $useIncludePath = false): ?string|false {}
public function diagnose(): ?bool {}
public function getRelease(): ?string {}
public function getConfig(): ?array {}
public function getStatus(): ?int {}
public function getHtmlVer(): ?int {}
public function getOptDoc(string $option): ?string|false {}
public function isXhtml(): ?bool {}
public function isXml(): ?bool {}
public function root(): ??tidyNode {}
public function head(): ??tidyNode {}
public function html(): ??tidyNode {}
public function body(): ??tidyNode {}
}
final class tidyNode {
private function __construct() {}
public function hasChildren(): bool {}
public function hasSiblings(): bool {}
public function isComment(): bool {}
public function isHtml(): bool {}
public function isText(): bool {}
public function isJste(): bool {}
public function isAsp(): bool {}
public function isPhp(): bool {}
public function getParent(): ?tidyNode {}
}
class PhpToken {
public static function tokenize(string $code, int $flags = 0): array {}
final public function __construct(int $id, string $text, int $line = -1, int $pos = -1) {}
public function is($kind): bool {}
public function isIgnorable(): bool {}
public function getTokenName(): ?string {}
public function __toString(): string {}
}
class XMLReader {
public function close() {}
public function getAttribute(string $name): ??string {}
public function getAttributeNo(int $index): ??string {}
public function getAttributeNs(string $name, string $namespace): ??string {}
public function getParserProperty(int $property): ?bool {}
public function isValid(): ?bool {}
public function lookupNamespace(string $prefix): ??string {}
public function moveToAttribute(string $name): ?bool {}
public function moveToAttributeNo(int $index): ?bool {}
public function moveToAttributeNs(string $name, string $namespace): ?bool {}
public function moveToElement(): ?bool {}
public function moveToFirstAttribute(): ?bool {}
public function moveToNextAttribute(): ?bool {}
public function read(): ?bool {}
public function next(?string $name = NULL): ?bool {}
public static function open(string $uri, ?string $encoding = NULL, int $flags = 0) {}
public function readInnerXml(): ?string {}
public function readOuterXml(): ?string {}
public function readString(): ?string {}
public function setSchema(?string $filename): ?bool {}
public function setParserProperty(int $property, bool $value): ?bool {}
public function setRelaxNGSchema(?string $filename): ?bool {}
public function setRelaxNGSchemaSource(?string $source): ?bool {}
public static function XML(string $source, ?string $encoding = NULL, int $flags = 0) {}
public function expand(?DOMNode $baseNode = NULL): ?DOMNode|false {}
}
class XMLWriter {
public function openUri(string $uri): ?bool {}
public function openMemory(): ?bool {}
public function setIndent(bool $enable): ?bool {}
public function setIndentString(string $indentation): ?bool {}
public function startComment(): ?bool {}
public function endComment(): ?bool {}
public function startAttribute(string $name): ?bool {}
public function endAttribute(): ?bool {}
public function writeAttribute(string $name, string $value): ?bool {}
public function startAttributeNs(?string $prefix, string $name, ?string $namespace): ?bool {}
public function writeAttributeNs(?string $prefix, string $name, ?string $namespace, string $value): ?bool {}
public function startElement(string $name): ?bool {}
public function endElement(): ?bool {}
public function fullEndElement(): ?bool {}
public function startElementNs(?string $prefix, string $name, ?string $namespace): ?bool {}
public function writeElement(string $name, ?string $content = NULL): ?bool {}
public function writeElementNs(?string $prefix, string $name, ?string $namespace, ?string $content = NULL): ?bool {}
public function startPi(string $target): ?bool {}
public function endPi(): ?bool {}
public function writePi(string $target, string $content): ?bool {}
public function startCdata(): ?bool {}
public function endCdata(): ?bool {}
public function writeCdata(string $content): ?bool {}
public function text(string $content): ?bool {}
public function writeRaw(string $content): ?bool {}
public function startDocument(?string $version = '1.0', ?string $encoding = NULL, ?string $standalone = NULL): ?bool {}
public function endDocument(): ?bool {}
public function writeComment(string $content): ?bool {}
public function startDtd(string $qualifiedName, ?string $publicId = NULL, ?string $systemId = NULL): ?bool {}
public function endDtd(): ?bool {}
public function writeDtd(string $name, ?string $publicId = NULL, ?string $systemId = NULL, ?string $content = NULL): ?bool {}
public function startDtdElement(string $qualifiedName): ?bool {}
public function endDtdElement(): ?bool {}
public function writeDtdElement(string $name, string $content): ?bool {}
public function startDtdAttlist(string $name): ?bool {}
public function endDtdAttlist(): ?bool {}
public function writeDtdAttlist(string $name, string $content): ?bool {}
public function startDtdEntity(string $name, bool $isParam): ?bool {}
public function endDtdEntity(): ?bool {}
public function writeDtdEntity(string $name, string $content, bool $isParam = false, ?string $publicId = NULL, ?string $systemId = NULL, ?string $notationData = NULL): ?bool {}
public function outputMemory(bool $flush = true): ?string {}
public function flush(bool $empty = true): ?string|int {}
}
class XSLTProcessor {
public function importStylesheet(object $stylesheet): ?bool {}
public function transformToDoc(object $document, ?string $returnClass = NULL): ?DOMDocument|false {}
public function transformToUri(object $document, string $uri): ?int {}
public function transformToXml(object $document): ?string|false|null {}
public function setParameter(string $namespace, array|string $name, ?string $value = NULL): ?bool {}
public function getParameter(string $namespace, string $name): ?string|false {}
public function removeParameter(string $namespace, string $name): ?bool {}
public function hasExsltSupport(): ?bool {}
public function registerPHPFunctions(array|string|null $functions = NULL): ?void {}
public function setProfiling(?string $filename) {}
public function setSecurityPrefs(int $preferences): ?int {}
public function getSecurityPrefs(): ?int {}
}
class ZipArchive {
public function open(string $filename, int $flags = 0): ?int|bool {}
public function setPassword(string $password): ?bool {}
public function close(): ?bool {}
public function count(): ?int {}
public function getStatusString(): ?string {}
public function clearError(): void {}
public function addEmptyDir(string $dirname, int $flags = 0): ?bool {}
public function addFromString(string $name, string $content, int $flags = 8192): ?bool {}
public function addFile(string $filepath, string $entryname = '', int $start = 0, int $length = 0, int $flags = 8192): ?bool {}
public function replaceFile(string $filepath, int $index, int $start = 0, int $length = 0, int $flags = 0): ?bool {}
public function addGlob(string $pattern, int $flags = 0, array $options = array (
)): ?array|false {}
public function addPattern(string $pattern, string $path = '.', array $options = array (
)): ?array|false {}
public function renameIndex(int $index, string $new_name): ?bool {}
public function renameName(string $name, string $new_name): ?bool {}
public function setArchiveComment(string $comment): ?bool {}
public function getArchiveComment(int $flags = 0): ?string|false {}
public function setCommentIndex(int $index, string $comment): ?bool {}
public function setCommentName(string $name, string $comment): ?bool {}
public function setMtimeIndex(int $index, int $timestamp, int $flags = 0): ?bool {}
public function setMtimeName(string $name, int $timestamp, int $flags = 0): ?bool {}
public function getCommentIndex(int $index, int $flags = 0): ?string|false {}
public function getCommentName(string $name, int $flags = 0): ?string|false {}
public function deleteIndex(int $index): ?bool {}
public function deleteName(string $name): ?bool {}
public function statName(string $name, int $flags = 0): ?array|false {}
public function statIndex(int $index, int $flags = 0): ?array|false {}
public function locateName(string $name, int $flags = 0): ?int|false {}
public function getNameIndex(int $index, int $flags = 0): ?string|false {}
public function unchangeArchive(): ?bool {}
public function unchangeAll(): ?bool {}
public function unchangeIndex(int $index): ?bool {}
public function unchangeName(string $name): ?bool {}
public function extractTo(string $pathto, array|string|null $files = NULL): ?bool {}
public function getFromName(string $name, int $len = 0, int $flags = 0): ?string|false {}
public function getFromIndex(int $index, int $len = 0, int $flags = 0): ?string|false {}
public function getStreamIndex(int $index, int $flags = 0) {}
public function getStreamName(string $name, int $flags = 0) {}
public function getStream(string $name) {}
public function setExternalAttributesName(string $name, int $opsys, int $attr, int $flags = 0): ?bool {}
public function setExternalAttributesIndex(int $index, int $opsys, int $attr, int $flags = 0): ?bool {}
public function getExternalAttributesName(string $name, $opsys, $attr, int $flags = 0): ?bool {}
public function getExternalAttributesIndex(int $index, $opsys, $attr, int $flags = 0): ?bool {}
public function setCompressionName(string $name, int $method, int $compflags = 0): ?bool {}
public function setCompressionIndex(int $index, int $method, int $compflags = 0): ?bool {}
public function setEncryptionName(string $name, int $method, ?string $password = NULL): ?bool {}
public function setEncryptionIndex(int $index, int $method, ?string $password = NULL): ?bool {}
public function registerProgressCallback(float $rate, callable $callback): ?bool {}
public function registerCancelCallback(callable $callback): ?bool {}
public static function isCompressionMethodSupported(int $method, bool $enc = true): bool {}
public static function isEncryptionMethodSupported(int $method, bool $enc = true): bool {}
}
class Memcached {
public function __construct(?string $persistent_id = NULL, ?callable $callback = NULL, ?string $connection_str = NULL) {}
public function getResultCode(): int {}
public function getResultMessage(): string {}
public function get(string $key, ?callable $cache_cb = NULL, int $get_flags = 0): mixed {}
public function getByKey(string $server_key, string $key, ?callable $cache_cb = NULL, int $get_flags = 0): mixed {}
public function getMulti(array $keys, int $get_flags = 0): array|false {}
public function getMultiByKey(string $server_key, array $keys, int $get_flags = 0): array|false {}
public function getDelayed(array $keys, bool $with_cas = false, ?callable $value_cb = NULL): bool {}
public function getDelayedByKey(string $server_key, array $keys, bool $with_cas = false, ?callable $value_cb = NULL): bool {}
public function fetch(): array|false {}
public function fetchAll(): array|false {}
public function set(string $key, mixed $value, int $expiration = 0): bool {}
public function setByKey(string $server_key, string $key, mixed $value, int $expiration = 0): bool {}
public function touch(string $key, int $expiration = 0): bool {}
public function touchByKey(string $server_key, string $key, int $expiration = 0): bool {}
public function setMulti(array $items, int $expiration = 0): bool {}
public function setMultiByKey(string $server_key, array $items, int $expiration = 0): bool {}
public function cas(string $cas_token, string $key, mixed $value, int $expiration = 0): bool {}
public function casByKey(string $cas_token, string $server_key, string $key, mixed $value, int $expiration = 0): bool {}
public function add(string $key, mixed $value, int $expiration = 0): bool {}
public function addByKey(string $server_key, string $key, mixed $value, int $expiration = 0): bool {}
public function append(string $key, string $value): ?bool {}
public function appendByKey(string $server_key, string $key, string $value): ?bool {}
public function prepend(string $key, string $value): ?bool {}
public function prependByKey(string $server_key, string $key, string $value): ?bool {}
public function replace(string $key, mixed $value, int $expiration = 0): bool {}
public function replaceByKey(string $server_key, string $key, mixed $value, int $expiration = 0): bool {}
public function delete(string $key, int $time = 0): bool {}
public function deleteMulti(array $keys, int $time = 0): array {}
public function deleteByKey(string $server_key, string $key, int $time = 0): bool {}
public function deleteMultiByKey(string $server_key, array $keys, int $time = 0): array {}
public function increment(string $key, int $offset = 1, int $initial_value = 0, int $expiry = 0): int|false {}
public function decrement(string $key, int $offset = 1, int $initial_value = 0, int $expiry = 0): int|false {}
public function incrementByKey(string $server_key, string $key, int $offset = 1, int $initial_value = 0, int $expiry = 0): int|false {}
public function decrementByKey(string $server_key, string $key, int $offset = 1, int $initial_value = 0, int $expiry = 0): int|false {}
public function addServer(string $host, int $port, int $weight = 0): bool {}
public function addServers(array $servers): bool {}
public function getServerList(): array {}
public function getServerByKey(string $server_key): array|false {}
public function resetServerList(): bool {}
public function quit(): bool {}
public function flushBuffers(): bool {}
public function getLastErrorMessage(): string {}
public function getLastErrorCode(): int {}
public function getLastErrorErrno(): int {}
public function getLastDisconnectedServer(): array|false {}
public function getStats(?string $type = NULL): array|false {}
public function getVersion(): array|false {}
public function getAllKeys(): array|false {}
public function flush(int $delay = 0): bool {}
public function getOption(int $option): mixed {}
public function setOption(int $option, mixed $value): bool {}
public function setOptions(array $options): bool {}
public function setBucket(array $host_map, ?array $forward_map, int $replicas): bool {}
public function setSaslAuthData(string $username, string $password): bool {}
public function setEncodingKey(string $key): bool {}
public function isPersistent(): bool {}
public function isPristine(): bool {}
public function checkKey(string $key): bool {}
}
class MemcachedException {
public function __construct(string $message = '', int $code = 0, ?Throwable $previous = NULL) {}
public function __wakeup(): ?void {}
final public function getMessage(): string {}
final public function getCode() {}
final public function getFile(): string {}
final public function getLine(): int {}
final public function getTrace(): array {}
final public function getPrevious(): ?Throwable {}
final public function getTraceAsString(): string {}
public function __toString(): string {}
}
interface Traversable {
}
interface IteratorAggregate {
public function getIterator(): ?Traversable {}
}
interface Iterator {
public function current(): ?mixed {}
public function next(): ?void {}
public function key(): ?mixed {}
public function valid(): ?bool {}
public function rewind(): ?void {}
}
interface Serializable {
public function serialize() {}
public function unserialize(string $data) {}
}
interface ArrayAccess {
public function offsetExists(mixed $offset): ?bool {}
public function offsetGet(mixed $offset): ?mixed {}
public function offsetSet(mixed $offset, mixed $value): ?void {}
public function offsetUnset(mixed $offset): ?void {}
}
interface Countable {
public function count(): ?int {}
}
interface Stringable {
public function __toString(): string {}
}
interface Throwable {
public function getMessage(): string {}
public function getCode() {}
public function getFile(): string {}
public function getLine(): int {}
public function getTrace(): array {}
public function getPrevious(): ?Throwable {}
public function getTraceAsString(): string {}
public function __toString(): string {}
}
interface UnitEnum {
public static function cases(): array {}
}
interface BackedEnum {
public static function from(string|int $value): static {}
public static function tryFrom(string|int $value): ?static {}
public static function cases(): array {}
}
interface DateTimeInterface {
public function format(string $format): ?string {}
public function getTimezone(): ?DateTimeZone|false {}
public function getOffset(): ?int {}
public function getTimestamp(): ?int {}
public function diff(DateTimeInterface $targetObject, bool $absolute = false): ?DateInterval {}
public function __wakeup(): ?void {}
public function __serialize(): array {}
public function __unserialize(array $data): void {}
}
interface JsonSerializable {
public function jsonSerialize(): ?mixed {}
}
namespace Random {
interface Engine {
public function generate(): string {}
}
}
namespace Random {
interface CryptoSafeEngine {
public function generate(): string {}
}
}
interface Reflector {
public function __toString(): string {}
}
interface RecursiveIterator {
public function hasChildren(): ?bool {}
public function getChildren(): ??RecursiveIterator {}
public function current(): ?mixed {}
public function next(): ?void {}
public function key(): ?mixed {}
public function valid(): ?bool {}
public function rewind(): ?void {}
}
interface OuterIterator {
public function getInnerIterator(): ??Iterator {}
public function current(): ?mixed {}
public function next(): ?void {}
public function key(): ?mixed {}
public function valid(): ?bool {}
public function rewind(): ?void {}
}
interface SeekableIterator {
public function seek(int $offset): ?void {}
public function current(): ?mixed {}
public function next(): ?void {}
public function key(): ?mixed {}
public function valid(): ?bool {}
public function rewind(): ?void {}
}
interface SplObserver {
public function update(SplSubject $subject): ?void {}
}
interface SplSubject {
public function attach(SplObserver $observer): ?void {}
public function detach(SplObserver $observer): ?void {}
public function notify(): ?void {}
}
interface SessionHandlerInterface {
public function open(string $path, string $name): ?bool {}
public function close(): ?bool {}
public function read(string $id): ?string|false {}
public function write(string $id, string $data): ?bool {}
public function destroy(string $id): ?bool {}
public function gc(int $max_lifetime): ?int|false {}
}
interface SessionIdInterface {
public function create_sid(): ?string {}
}
interface SessionUpdateTimestampHandlerInterface {
public function validateId(string $id): ?bool {}
public function updateTimestamp(string $id, string $data): ?bool {}
}
interface DOMParentNode {
public function append($nodes): void {}
public function prepend($nodes): void {}
}
interface DOMChildNode {
public function remove(): void {}
public function before($nodes): void {}
public function after($nodes): void {}
public function replaceWith($nodes): void {}
}
