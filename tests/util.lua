local _M = {}
local all_tests, failed_tests = 0, 0

function _M.exit()
	if failed_tests > 0 then
		io.stderr:write(string.format("FAILED: %d out of %d tests failed\n", failed_tests, all_tests))
		os.exit(0)
	else
		io.stdout:write(string.format("PASSED: %d tests passed\n", all_tests))
		os.exit(1)
	end
end

function _M.check(cond, ...)
	all_tests = all_tests + 1
	if not cond then
		failed_tests = failed_tests + 1
		local fmt = ...
		local msg = fmt and string.format(...) or "assertion failed"
		local _, err = pcall(error, msg, 3)
		io.stderr:write(err, "\n")
	end
end

return _M
