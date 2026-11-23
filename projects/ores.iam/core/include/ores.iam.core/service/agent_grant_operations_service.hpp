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
#ifndef ORES_IAM_CORE_SERVICE_AGENT_GRANT_OPERATIONS_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_AGENT_GRANT_OPERATIONS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/agent_grant.hpp"
#include "ores.iam.api/messaging/agent_grant_operations_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/logger.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/service/rate_limiter.hpp"
#include <chrono>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::service {

/**
 * @brief Creates and revokes LLM agent grants and their keys, lists a person's
 * own, and exchanges a key for a token naming the person and the agent.
 *
 * The grant and key tables are readable through their generated protocols and
 * written only here. The rules are those of Agent grants and Delegation tokens
 * for LLM agents in doc/knowledge/agents: a grant names a role the person
 * holds, explicitly and with nothing withheld from agents; it lasts at most
 * ninety days; its key is shown once and stored as a hash; and the exchange
 * re-checks the key, the grant and the person every time.
 */
class ORES_IAM_CORE_EXPORT agent_grant_operations_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.agent_grant_operations_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static constexpr std::string_view create_permission = "iam::agent_grants:create";
    static constexpr std::string_view revoke_permission = "iam::agent_grants:revoke";
    static constexpr std::string_view read_permission = "iam::agent_grants:read";
    static constexpr std::string_view exchange_permission = "iam::agent_keys:exchange";
    static constexpr int max_valid_days = 90;
    static constexpr std::chrono::minutes token_lifetime{5};
    static constexpr std::chrono::minutes session_idle_limit{30};

    explicit agent_grant_operations_service(
        const ores::database::context& ctx,
        std::optional<security::jwt::jwt_authenticator> signer = std::nullopt);

    messaging::create_agent_grant_response
    create_agent_grant(const messaging::create_agent_grant_request& request);

    messaging::create_agent_key_response
    create_agent_key(const messaging::create_agent_key_request& request);

    messaging::revoke_agent_grant_response
    revoke_agent_grant(const messaging::revoke_agent_grant_request& request);

    messaging::revoke_agent_key_response
    revoke_agent_key(const messaging::revoke_agent_key_request& request);

    /**
     * @brief Ends an agent's session by revoking the key that opened it.
     *
     * Ending the session alone would not stop the agent, whose next exchange
     * would open another, so the key is revoked under the same rule as
     * revoke_agent_key: the grant's person, or a holder of the revoke
     * permission. Revoking a key ends its open session, and this session too.
     */
    messaging::revoke_agent_key_response end_agent_session(const std::string& session_id);

    messaging::list_own_agent_grants_response
    list_own_agent_grants(const messaging::list_own_agent_grants_request& request);

    messaging::get_agent_held_write_response
    get_agent_held_write(const messaging::get_agent_held_write_request& request);

    messaging::list_own_agent_activity_response
    list_own_agent_activity(const messaging::list_own_agent_activity_request& request);

    messaging::list_every_agent_grant_response
    list_every_agent_grant(const messaging::list_every_agent_grant_request& request);

    messaging::exchange_agent_key_response
    exchange_agent_key(const messaging::exchange_agent_key_request& request);

    /**
     * @brief The token that replays an approved agent write, as the person
     * with the agent as actor.
     */
    messaging::issue_agent_replay_token_response
    issue_agent_replay_token(const messaging::issue_agent_replay_token_request& request);

private:
    struct limiters {
        limiters();
        ores::service::service::rate_limiter per_key;
        ores::service::service::rate_limiter per_tenant;
    };

    static const std::shared_ptr<limiters>& shared_limiters();

    /**
     * @brief Makes a key for a grant, saves its hash, and returns the key, the
     * only time it exists outside the agent.
     */
    std::string mint_key(const ores::database::context& grant_ctx,
                         const domain::agent_grant& grant,
                         boost::uuids::uuid& key_id);

    ores::database::context ctx_;
    std::optional<security::jwt::jwt_authenticator> signer_;
    std::shared_ptr<limiters> limiters_;
};

}

#endif
