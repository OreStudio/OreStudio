/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_IAM_CORE_REPOSITORY_AGENT_TOKEN_ISSUE_LOG_HPP
#define ORES_IAM_CORE_REPOSITORY_AGENT_TOKEN_ISSUE_LOG_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/logger.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::repository {

/**
 * @brief One exchange of an agent's key, issued or refused. Never the key.
 */
struct agent_token_issue_record final {
    std::chrono::system_clock::time_point issued_at;
    std::string tenant_id;
    std::string party_id;
    std::string grant_id;
    std::string key_id;
    std::string session_id;
    std::string grantor_account_id;
    std::string client_address;
    std::string outcome;
    std::string reason;
};

/**
 * @brief A key's last issued exchange: the session it named, and when.
 */
struct agent_key_last_issue final {
    std::string session_id;
    std::chrono::system_clock::time_point issued_at;
};

/**
 * @brief The agent token issue log: appends each exchange, and answers a key's
 * last issue, from which the exchange finds the key's open session.
 */
class ORES_IAM_CORE_EXPORT agent_token_issue_log {
private:
    inline static std::string_view logger_name = "ores.iam.repository.agent_token_issue_log";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    static constexpr std::string_view issued = "issued";
    static constexpr std::string_view refused = "refused";

    explicit agent_token_issue_log(const context& ctx);

    void record(const agent_token_issue_record& record);

    [[nodiscard]] std::optional<agent_key_last_issue> last_issue(const std::string& key_id) const;

private:
    context ctx_;
};

}

#endif
