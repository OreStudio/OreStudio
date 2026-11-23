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
#ifndef ORES_IAM_CORE_REPOSITORY_AGENT_CALL_LOG_HPP
#define ORES_IAM_CORE_REPOSITORY_AGENT_CALL_LOG_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/agent_call.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/logger.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::repository {

/**
 * @brief The log of every call an LLM agent made through ores.http: appends a
 * call, and reads a person's agents' calls, newest first.
 */
class ORES_IAM_CORE_EXPORT agent_call_log {
private:
    inline static std::string_view logger_name = "ores.iam.repository.agent_call_log";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit agent_call_log(const context& ctx);

    /**
     * @brief Appends a call, through a session of the call's own tenant.
     */
    void record(domain::agent_call call);

    /**
     * @brief The calls the agents of one person made, newest first, a page at
     * a time; with a grant, that grant's agent's calls only.
     */
    [[nodiscard]] std::vector<domain::agent_call> calls_for(const std::string& grantor_account_id,
                                                            const std::string& grant_id,
                                                            std::uint32_t offset,
                                                            std::uint32_t limit) const;

private:
    context ctx_;
};

}

#endif
