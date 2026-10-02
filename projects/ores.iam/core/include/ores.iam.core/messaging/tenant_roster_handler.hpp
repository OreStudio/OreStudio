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
#ifndef ORES_IAM_CORE_MESSAGING_TENANT_ROSTER_HANDLER_HPP
#define ORES_IAM_CORE_MESSAGING_TENANT_ROSTER_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/messaging/tenant_roster_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <cstdint>
#include <exception>
#include <optional>
#include <string>
#include <string_view>

namespace ores::iam::messaging {

/**
 * @brief Answers the tenant roster's search, iam.v1.tenants.search.
 *
 * The search, the two filters, the paging and the rule that leaves the system
 * tenant out are one SQL function, ores_iam_tenants_search_fn, so every reader
 * of the roster gets the same answer. The handler checks the caller may read
 * tenants and turns the rows into tenants.
 */
class tenant_roster_handler {
private:
    inline static std::string_view logger_name = "ores.iam.messaging.tenant_roster_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /// The most tenants one page may ask for.
    static constexpr std::uint32_t max_limit = 1000;

    /*
     * Where the search function puts the two columns read by position: the
     * id, which is null on the one row a page past the last match returns,
     * and the total every row carries. The others are read in to_tenant in
     * the function's column order.
     */
    static constexpr std::size_t id_column = 0;
    static constexpr std::size_t recorded_at_column = 13;
    static constexpr std::size_t total_column = 14;
    static constexpr std::size_t column_count = total_column + 1;

public:
    tenant_roster_handler(ores::nats::service::client& nats,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void search(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::reply;
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "iam::tenants:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        const auto parsed = ores::service::messaging::decode<search_tenants_request>(msg);
        if (!parsed) {
            reply(nats_,
                  msg,
                  search_tenants_response{.success = false,
                                          .message = "The request could not be read."});
            return;
        }
        const auto& req = *parsed;

        search_tenants_response resp;
        try {
            using namespace ores::database::repository;
            const auto limit = std::clamp(req.limit, std::uint32_t{1}, max_limit);
            const auto rows = execute_parameterized_multi_column_query(
                ctx,
                "SELECT * FROM ores_iam_tenants_search_fn($1, $2, $3, $4, $5)",
                {req.search,
                 req.type_filter,
                 req.status_filter,
                 std::to_string(limit),
                 std::to_string(req.offset)},
                lg(),
                "searching tenants");
            for (const auto& row : rows) {
                if (row.size() < column_count)
                    continue;
                resp.total = std::stoull(row[total_column].value_or("0"));
                if (row[id_column])
                    resp.tenants.push_back(to_tenant(row));
            }
            resp.success = true;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    /**
     * @brief One search row as a tenant.
     *
     * The registry is system-scoped, so every row's owner is the system tenant;
     * the row's own identity is its id.
     */
    static domain::tenant to_tenant(const std::vector<std::optional<std::string>>& row) {
        using ores::database::repository::text_to_bool;
        using ores::database::repository::timestamp_to_timepoint;
        domain::tenant t;
        t.id = boost::lexical_cast<boost::uuids::uuid>(row[id_column].value_or(""));
        t.version = std::stoi(row[1].value_or("0"));
        t.code = row[2].value_or("");
        t.name = row[3].value_or("");
        t.type = row[4].value_or("");
        t.description = row[5].value_or("");
        t.hostname = row[6].value_or("");
        t.status = row[7].value_or("");
        t.is_registration_default = text_to_bool(row[8]);
        t.modified_by = row[9].value_or("");
        t.performed_by = row[10].value_or("");
        t.change_reason_code = row[11].value_or("");
        t.change_commentary = row[12].value_or("");
        t.recorded_at =
            timestamp_to_timepoint(std::string_view(row[recorded_at_column].value_or("")));
        return t;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
