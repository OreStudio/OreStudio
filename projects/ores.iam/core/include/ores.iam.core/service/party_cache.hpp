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
#ifndef ORES_IAM_SERVICE_PARTY_CACHE_HPP
#define ORES_IAM_SERVICE_PARTY_CACHE_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include <boost/container_hash/hash.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <rfl/json.hpp>
#include <shared_mutex>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::iam::service {

namespace {
inline auto& party_cache_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.service.party_cache");
    return instance;
}
} // namespace

/**
 * @brief In-process per-tenant cache of refdata party data.
 *
 * Populated via NATS request to refdata.v1.parties.read at startup and
 * reloaded on receipt of refdata.v1.parties.changed notifications.
 * Eliminates direct cross-service DB reads from IAM to refdata party tables.
 */
class party_cache {
    using uuid_hash = boost::hash<boost::uuids::uuid>;
    using party_map =
        std::unordered_map<boost::uuids::uuid, ores::refdata::domain::party, uuid_hash>;
    using children_map =
        std::unordered_map<boost::uuids::uuid, std::vector<boost::uuids::uuid>, uuid_hash>;

    struct partition_t {
        party_map parties;
        children_map children; // parent_id → [child_id, ...]
    };

public:
    explicit party_cache(ores::nats::service::client& nats)
        : nats_(nats) {}

    void load(const std::string& tenant_id) {
        using namespace ores::logging;
        try {
            const auto req_json = rfl::json::write(
                ores::refdata::messaging::read_parties_for_cache_request{.tenant_id = tenant_id});
            const auto reply = nats_.request_sync(
                ores::refdata::messaging::read_parties_for_cache_request::nats_subject,
                ores::nats::as_bytes(req_json));
            auto resp = rfl::json::read<ores::refdata::messaging::read_parties_for_cache_response>(
                ores::nats::as_string_view(reply.data));
            if (!resp || !resp->success) {
                const auto msg = resp ? resp->message : "parse error";
                BOOST_LOG_SEV(party_cache_lg(), warn)
                    << "Party cache load failed for tenant " << tenant_id << ": " << msg;
                return;
            }
            partition_t data;
            data.parties.reserve(resp->parties.size());
            for (auto& p : resp->parties) {
                if (p.parent_party_id)
                    data.children[*p.parent_party_id].push_back(p.id);
                data.parties.emplace(p.id, std::move(p));
            }
            {
                std::unique_lock lock(mutex_);
                cache_[tenant_id] = std::move(data);
            }
            BOOST_LOG_SEV(party_cache_lg(), debug)
                << "Loaded " << resp->parties.size() << " parties for tenant " << tenant_id;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_cache_lg(), warn)
                << "Party cache load exception for tenant " << tenant_id << ": " << e.what();
        }
    }

    std::optional<ores::refdata::domain::party>
    lookup_party(const std::string& tenant_id, const boost::uuids::uuid& party_id) const {
        std::shared_lock lock(mutex_);
        const auto t_it = cache_.find(tenant_id);
        if (t_it == cache_.end())
            return std::nullopt;
        const auto p_it = t_it->second.parties.find(party_id);
        if (p_it == t_it->second.parties.end())
            return std::nullopt;
        return p_it->second;
    }

    std::vector<boost::uuids::uuid>
    compute_visible_party_ids(const std::string& tenant_id,
                              const boost::uuids::uuid& root_id) const {
        std::shared_lock lock(mutex_);
        const auto t_it = cache_.find(tenant_id);
        if (t_it == cache_.end())
            return {root_id};
        std::vector<boost::uuids::uuid> result;
        collect_subtree(t_it->second.children, root_id, result);
        if (result.empty())
            return {root_id};
        return result;
    }

private:
    void collect_subtree(const children_map& children,
                         const boost::uuids::uuid& node,
                         std::vector<boost::uuids::uuid>& out) const {
        out.push_back(node);
        const auto it = children.find(node);
        if (it != children.end())
            for (const auto& child : it->second)
                collect_subtree(children, child, out);
    }

    ores::nats::service::client& nats_;
    mutable std::shared_mutex mutex_;
    std::unordered_map<std::string, partition_t> cache_;
};

} // namespace ores::iam::service
#endif
