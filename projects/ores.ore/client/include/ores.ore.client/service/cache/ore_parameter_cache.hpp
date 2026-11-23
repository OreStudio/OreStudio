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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_event_cache.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ORE_CLIENT_SERVICE_CACHE_ORE_PARAMETER_CACHE_HPP
#define ORES_ORE_CLIENT_SERVICE_CACHE_ORE_PARAMETER_CACHE_HPP

#include "ores.eventing.core/service/cache/partition_token_cache.hpp"
#include "ores.eventing.core/service/cache/partitioned_cache.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.ore.api/domain/ore_parameter.hpp"
#include "ores.ore.api/messaging/ore_parameter_protocol.hpp"
#include <boost/container_hash/hash.hpp>
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <functional>
#include <immer/map.hpp>
#include <immer/map_transient.hpp>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::ore::service::cache {

namespace {
inline auto& ore_parameter_cache_lg() {
    static auto instance = ores::logging::make_logger("ores.ore.service.cache.ore_parameter_cache");
    return instance;
}
}

/**
 * @brief In-process per-tenant cache of ore ore_parameters data.
 *
 * Populated by paging ore's list_ore_parameters read to its
 * end at startup, and reloaded on receipt of a ore_parameter event
 * for the affected tenant (see the nats-event-cache.registrar_wiring
 * archetype). See the "Generic entity-mirror cache primitive + codegen
 * facet" story.
 *
 * Each partition is one tenant's copy, so it is read inside that tenant:
 * the @c token_provider gives a token for the partition's tenant, obtained
 * by Token Exchange (see partition_token_cache), and row-level security
 * returns exactly the rows that tenant may see. A target that answers
 * @c token_expired gets one renewed token and one retry. Omit the provider
 * only against a producer that has not opted into the auth check.
 */
class ore_parameter_cache {
    using key_hash = boost::hash<boost::uuids::uuid>;
    using analytic_parameters = std::vector<ores::ore::domain::ore_parameter>;
    using instrument_types =
        std::unordered_map<std::string, std::vector<ores::ore::domain::ore_parameter>>;
    struct parameter_views {
        analytic_parameters analytics;
        instrument_types instruments;
    };
    using cache_t =
        ores::eventing::service::cache::partitioned_cache<std::string,
                                                          boost::uuids::uuid,
                                                          ores::ore::domain::ore_parameter,
                                                          parameter_views,
                                                          key_hash>;

public:
    explicit ore_parameter_cache(
        ores::nats::service::client& nats,
        ores::eventing::service::cache::partition_token_provider token_provider = nullptr)
        : nats_(nats)
        , token_provider_(std::move(token_provider)) {}

    ore_parameter_cache(const ore_parameter_cache&) = delete;
    ore_parameter_cache& operator=(const ore_parameter_cache&) = delete;
    ore_parameter_cache(ore_parameter_cache&&) = delete;
    ore_parameter_cache& operator=(ore_parameter_cache&&) = delete;

    /**
     * @brief Arms (or replaces) the token provider after construction. For a
     * consumer that must construct this cache during its own Phase 1
     * (wiring/readiness) but can only mint a token during Phase 2
     * (post-readiness) — e.g. a self-referential consumer authenticating
     * against its own just-registered subjects. See "Service Bootstrap
     * Phases" in the architecture docs.
     */
    void
    set_token_provider(ores::eventing::service::cache::partition_token_provider token_provider) {
        token_provider_ = std::move(token_provider);
    }

    /**
     * @brief Loads (or reloads) one tenant's partition.
     *
     * A full read is the canonical list paged to its end: the protocol has
     * no bulk read, so the warm-up walks page by page until the response's
     * total is reached.
     *
     * @return An empty string on success. On failure, a human-readable
     * message describing what went wrong -- the failure is still logged
     * (@c warn) either way, but callers that can surface it to a user
     * (e.g. a UI's status/error signal) need it returned, not just
     * buried in a log a user never sees. Silently-swallowed background
     * failures are the right default for the warm-up/refresh path
     * (@c nats-event-cache.registrar_wiring calls this fire-and-forget
     * from a detached thread), but the return value lets any caller
     * that *can* react to a failure opt in.
     */
    [[nodiscard]] std::string load(const std::string& tenant_id) {
        using namespace ores::logging;
        try {
            const auto& codec = ores::nats::default_wire_codec();
            std::unordered_map<std::string, std::string> headers;
            const auto authorise = [&](bool renew) {
                if (!token_provider_)
                    return true;
                const auto token = token_provider_(tenant_id, renew);
                if (token.empty())
                    return false;
                headers[std::string(ores::nats::headers::authorization)] =
                    std::string(ores::nats::headers::bearer_prefix) + token;
                return true;
            };
            if (!authorise(false)) {
                const std::string msg = "no token acts inside this tenant";
                ORES_LOG_SEV(ore_parameter_cache_lg(), warn)
                    << "OreParameter cache load failed for tenant " << tenant_id << ": " << msg;
                return msg;
            }
            bool renewed = false;
            auto entries_t = cache_t::entries_map{}.transient();
            std::uint64_t count = 0;
            constexpr std::uint32_t page_size = 100;
            std::uint32_t offset = 0;
            while (true) {
                const auto bytes = codec.encode(ores::ore::messaging::list_ore_parameters_request{
                    .offset = offset, .limit = page_size});
                const auto reply = nats_.request_sync(
                    ores::ore::messaging::list_ore_parameters_request::nats_subject,
                    bytes,
                    headers);
                const auto refusal = reply.headers.find(std::string(ores::nats::headers::x_error));
                if (refusal != reply.headers.end() && refusal->second == "token_expired" &&
                    !renewed && authorise(true)) {
                    renewed = true;
                    continue;
                }
                auto resp =
                    codec.decode<ores::ore::messaging::list_ore_parameters_response>(reply.data);
                if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok) {
                    const auto msg =
                        resp ? resp->result.message : "parse error (malformed or error reply)";
                    ORES_LOG_SEV(ore_parameter_cache_lg(), warn)
                        << "OreParameter cache load failed for tenant " << tenant_id << ": " << msg;
                    return msg;
                }
                const auto page_count = resp->parameters.size();
                for (auto& v : resp->parameters)
                    entries_t.set(v.id, std::move(v));
                count += page_count;
                if (page_count == 0 || count >= resp->total)
                    break;
                offset += page_size;
            }
            auto entries = entries_t.persistent();
            parameter_views aux;
            for (const auto& [id, p] : entries) {
                if (p.scope == "analytic")
                    aux.analytics.push_back(p);
                else if (p.scope == "instrument")
                    aux.instruments[p.subtype].push_back(p);
            }
            cache_.replace_partition(tenant_id, entries, aux);
            ORES_LOG_SEV(ore_parameter_cache_lg(), debug)
                << "Loaded " << count << " ore_parameters for tenant " << tenant_id;
            return {};
        } catch (const std::exception& e) {
            ORES_LOG_SEV(ore_parameter_cache_lg(), warn)
                << "OreParameter cache load exception for tenant " << tenant_id << ": " << e.what();
            return e.what();
        }
    }

    std::optional<ores::ore::domain::ore_parameter> lookup(const std::string& tenant_id,
                                                           const boost::uuids::uuid& key) const {
        return cache_.get(tenant_id, key);
    }

    /**
     * @brief The parameters of the analytics the tenant's partition holds, empty
     * when the partition is not loaded.
     */
    analytic_parameters analytics_parameters(const std::string& tenant_id) const {
        const auto snap = cache_.snapshot(tenant_id);
        return snap ? snap->aux.analytics : analytic_parameters{};
    }

    /**
     * @brief The fields of the instruments the tenant's partition holds, by the
     * schema type they belong to, in no order (each field states its position),
     * empty when the partition is not loaded. The data element of each trade type is a field of the
     * type trade.
     */
    instrument_types instrument_fields(const std::string& tenant_id) const {
        const auto snap = cache_.snapshot(tenant_id);
        return snap ? snap->aux.instruments : instrument_types{};
    }

private:
    ores::nats::service::client& nats_;
    ores::eventing::service::cache::partition_token_provider token_provider_;
    cache_t cache_;
};

}

#endif
