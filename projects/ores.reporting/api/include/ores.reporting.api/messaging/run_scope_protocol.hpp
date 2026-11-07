// SPDX-License-Identifier: GPL-3.0-or-later
#ifndef ORES_REPORTING_API_MESSAGING_RUN_SCOPE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_RUN_SCOPE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::messaging {

/** Copies an existing owned report's scope into an empty, unshared configuration. */
struct copy_run_scope_request {
    using response_type = struct copy_run_scope_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.copy_run_scope";
    static constexpr bool requires_session = true;
    std::string source_definition_id;
    std::string target_definition_id;
};

struct copy_run_scope_response {
    bool success = false;
    std::string message;
    std::vector<std::string> book_ids;
    std::vector<std::string> portfolio_ids;
};

}
#endif
