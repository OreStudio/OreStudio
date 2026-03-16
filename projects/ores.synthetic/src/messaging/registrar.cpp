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
#include "ores.synthetic/messaging/registrar.hpp"

#include <optional>
#include <span>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/string_generator.hpp>
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.synthetic/messaging/generate_organisation_protocol.hpp"
#include "ores.synthetic/service/organisation_generator_service.hpp"

namespace ores::synthetic::messaging {

namespace {

template<typename Resp>
void reply(ores::nats::service::client& nats,
           const ores::nats::message& msg,
           const Resp& resp) {
    if (msg.reply_subject.empty())
        return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r)
        return std::nullopt;
    return *r;
}

} // namespace


std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    subs.push_back(nats.queue_subscribe(
        "synthetic.v1.>", "ores.synthetic.service",
        [&nats, base_ctx = ctx, verifier](ores::nats::message msg) mutable {
            auto ctx = [&]() -> ores::database::context {
                if (!verifier) return base_ctx;
                auto it = msg.headers.find("Authorization");
                if (it == msg.headers.end()) return base_ctx;
                const auto& val = it->second;
                if (!val.starts_with("Bearer ")) return base_ctx;
                const auto token = val.substr(7);
                auto claims = verifier->validate(token);
                if (!claims) return base_ctx;
                const auto tenant_id_str = claims->tenant_id.value_or("");
                if (tenant_id_str.empty()) return base_ctx;
                auto tid_result = ores::utility::uuid::tenant_id::from_string(tenant_id_str);
                if (!tid_result) return base_ctx;
                if (!claims->party_id || claims->party_id->empty())
                    return base_ctx.with_tenant(*tid_result, claims->username.value_or(""));
                try {
                    boost::uuids::string_generator sg;
                    boost::uuids::uuid party_id = sg(*claims->party_id);
                    std::vector<boost::uuids::uuid> visible_ids;
                    for (const auto& pid_str : claims->visible_party_ids)
                        visible_ids.push_back(sg(pid_str));
                    return base_ctx.with_party(*tid_result, party_id,
                        std::move(visible_ids), claims->username.value_or(""));
                } catch (...) {
                    return base_ctx.with_tenant(*tid_result, claims->username.value_or(""));
                }
            }();
            const auto& subj = msg.subject;

            // ----------------------------------------------------------------
            // Organisation — generate
            // ----------------------------------------------------------------
            if (subj.ends_with(".organisation.generate")) {
                if (auto req = decode<generate_organisation_request>(msg)) {
                    try {
                        domain::organisation_generation_options opts;
                        opts.country = req->country;
                        opts.party_count = req->party_count;
                        opts.counterparty_count = req->counterparty_count;
                        opts.portfolio_leaf_count = req->portfolio_leaf_count;
                        opts.books_per_leaf_portfolio =
                            req->books_per_leaf_portfolio;
                        opts.business_unit_count = req->business_unit_count;
                        opts.generate_addresses = req->generate_addresses;
                        opts.generate_identifiers = req->generate_identifiers;

                        service::organisation_generator_service svc;
                        const auto org = svc.generate(opts);

                        generate_organisation_response resp;
                        resp.success = true;
                        resp.parties_count =
                            static_cast<int>(org.parties.size());
                        resp.counterparties_count =
                            static_cast<int>(org.counterparties.size());
                        resp.business_unit_types_count =
                            static_cast<int>(org.business_unit_types.size());
                        resp.business_units_count =
                            static_cast<int>(org.business_units.size());
                        resp.portfolios_count =
                            static_cast<int>(org.portfolios.size());
                        resp.books_count =
                            static_cast<int>(org.books.size());
                        resp.contacts_count =
                            static_cast<int>(org.party_contacts.size() +
                                org.counterparty_contacts.size());
                        resp.identifiers_count =
                            static_cast<int>(org.party_identifiers.size() +
                                org.counterparty_identifiers.size());
                        reply(nats, msg, resp);
                    } catch (const std::exception& e) {
                        reply(nats, msg, generate_organisation_response{
                            .success = false, .error_message = e.what()});
                    }
                }
            }
        }));

    return subs;
}

} // namespace ores::synthetic::messaging
