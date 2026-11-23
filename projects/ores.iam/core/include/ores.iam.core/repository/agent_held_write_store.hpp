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
#ifndef ORES_IAM_CORE_REPOSITORY_AGENT_HELD_WRITE_STORE_HPP
#define ORES_IAM_CORE_REPOSITORY_AGENT_HELD_WRITE_STORE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/agent_held_write.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/logger.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::repository {

/**
 * @brief The writes LLM agents made, held until their people approve them.
 *
 * A write moves from held to applying when ores.http claims it to replay, and
 * from applying to applied or failed when the replay answers. Only a held row
 * can be claimed; ores.http hears approvals in a queue group, so one replica
 * takes each approval and no two replay one write.
 */
class ORES_IAM_CORE_EXPORT agent_held_write_store {
private:
    inline static std::string_view logger_name = "ores.iam.repository.agent_held_write_store";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    static constexpr std::string_view held = "held";
    static constexpr std::string_view applying = "applying";
    static constexpr std::string_view applied = "applied";
    static constexpr std::string_view failed = "failed";

    explicit agent_held_write_store(const context& ctx);

    /**
     * @brief Keeps a write, in its own tenant, as held.
     */
    void hold(domain::agent_held_write write);

    [[nodiscard]] std::optional<domain::agent_held_write> find(const std::string& request_id) const;

    /**
     * @brief Takes a held write to replay it; false when it is not held.
     */
    [[nodiscard]] bool claim(const std::string& request_id) const;

    /**
     * @brief Records how the replay of a claimed write answered.
     */
    void finish(const std::string& request_id,
                std::string_view state,
                int status,
                const std::string& outcome) const;

private:
    context ctx_;
};

}

#endif
