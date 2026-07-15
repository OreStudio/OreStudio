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
#ifndef ORES_IAM_CORE_SERVICE_CACHE_PARTY_CACHE_HPP
#define ORES_IAM_CORE_SERVICE_CACHE_PARTY_CACHE_HPP

#include "ores.eventing.core/service/cache/partitioned_cache.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/container_hash/hash.hpp>
#include <boost/uuid/uuid.hpp>
#include <functional>
#include <immer/map.hpp>
#include <immer/map_transient.hpp>
#include <optional>
#include <rfl/json.hpp>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::iam::service::cache {

namespace {
inline auto& party_cache_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.service.cache.party_cache");
    return instance;
}
} // namespace

/**
 * @brief In-process per-tenant cache of refdata parties data.
 *
 * Populated via NATS request to refdata's read_parties_for_cache
 * subject at startup, and reloaded on receipt of a party
 * changed-event notification for the affected tenant (see the
 * nats-event-cache.registrar_wiring archetype). See the "Generic
 * entity-mirror cache primitive + codegen facet" story.
 *
 * read_parties_for_cache requires a valid signed JWT (see the
 * nats-handler archetype); pass a @c token_provider — typically
 * ores::iam::client::make_service_token_provider's return value — so
 * every load() attaches a fresh service-account Bearer token. Omit it
 * only against a producer that has not opted into the auth check.
 */
class party_cache {
    using key_hash = boost::hash<boost::uuids::uuid>;
    using children_map = immer::map<boost::uuids::uuid, std::vector<boost::uuids::uuid>, key_hash>;
    using cache_t = ores::eventing::service::cache::partitioned_cache<std::string,
                                                                      boost::uuids::uuid,
                                                                      ores::refdata::domain::party,
                                                                      children_map,
                                                                      key_hash>;

public:
    explicit party_cache(ores::nats::service::client& nats,
                         std::function<std::string(bool)> token_provider = nullptr)
        : nats_(nats)
        , token_provider_(std::move(token_provider)) {}

    party_cache(const party_cache&) = delete;
    party_cache& operator=(const party_cache&) = delete;
    party_cache(party_cache&&) = delete;
    party_cache& operator=(party_cache&&) = delete;

    /**
     * @brief Arms (or replaces) the token provider after construction. For a
     * consumer that must construct this cache during its own Phase 1
     * (wiring/readiness) but can only mint a token during Phase 2
     * (post-readiness) — e.g. a self-referential consumer authenticating
     * against its own just-registered subjects. See "Service Bootstrap
     * Phases" in the architecture docs.
     */
    void set_token_provider(std::function<std::string(bool)> token_provider) {
        token_provider_ = std::move(token_provider);
    }

    void load(const std::string& tenant_id) {
        using namespace ores::logging;
        try {
            const auto req_json = rfl::json::write(
                ores::refdata::messaging::read_parties_for_cache_request{.tenant_id = tenant_id});
            std::unordered_map<std::string, std::string> headers;
            if (token_provider_)
                headers[std::string(ores::nats::headers::authorization)] =
                    std::string(ores::nats::headers::bearer_prefix) + token_provider_(false);
            const auto reply = nats_.request_sync(
                ores::refdata::messaging::read_parties_for_cache_request::nats_subject,
                ores::nats::as_bytes(req_json),
                std::move(headers));
            auto resp = rfl::json::read<ores::refdata::messaging::read_parties_for_cache_response>(
                ores::nats::as_string_view(reply.data));
            if (!resp || !resp->success) {
                const auto msg = resp ? resp->message : "parse error";
                BOOST_LOG_SEV(party_cache_lg(), warn)
                    << "Party cache load failed for tenant " << tenant_id << ": " << msg;
                return;
            }
            auto entries_t = cache_t::entries_map{}.transient();
            const auto count = resp->parties.size();
            for (auto& v : resp->parties)
                entries_t.set(v.id, std::move(v));
            auto entries = entries_t.persistent();
            auto children_t = children_map{}.transient();
            for (const auto& [id, p] : entries) {
                if (p.parent_party_id) {
                    const auto* existing = children_t.find(*p.parent_party_id);
                    auto siblings = existing ? *existing : std::vector<boost::uuids::uuid>{};
                    siblings.push_back(id);
                    children_t.set(*p.parent_party_id, std::move(siblings));
                }
            }
            auto aux = children_t.persistent();
            cache_.replace_partition(tenant_id, entries, aux);
            BOOST_LOG_SEV(party_cache_lg(), debug)
                << "Loaded " << count << " parties for tenant " << tenant_id;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_cache_lg(), warn)
                << "Party cache load exception for tenant " << tenant_id << ": " << e.what();
        }
    }

    std::optional<ores::refdata::domain::party> lookup(const std::string& tenant_id,
                                                       const boost::uuids::uuid& key) const {
        return cache_.get(tenant_id, key);
    }

    std::vector<boost::uuids::uuid>
    compute_visible_party_ids(const std::string& tenant_id,
                              const boost::uuids::uuid& root_id) const {
        const auto snap = cache_.snapshot(tenant_id);
        if (!snap)
            return {root_id};
        std::vector<boost::uuids::uuid> result;
        std::vector<boost::uuids::uuid> stack{root_id};
        while (!stack.empty()) {
            const auto node = stack.back();
            stack.pop_back();
            result.push_back(node);
            const auto* siblings = snap->aux.find(node);
            if (siblings)
                for (const auto& child : *siblings)
                    stack.push_back(child);
        }
        return result;
    }

private:
    ores::nats::service::client& nats_;
    std::function<std::string(bool)> token_provider_;
    cache_t cache_;
};

} // namespace ores::iam::service::cache

#endif
