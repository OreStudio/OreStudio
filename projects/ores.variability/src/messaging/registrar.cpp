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
#include "ores.variability/messaging/registrar.hpp"

#include <span>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.variability/service/feature_flags_service.hpp"

namespace ores::variability::messaging {

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
    context_extractor_fn context_extractor) {
    std::vector<ores::nats::service::subscription> subs;

    subs.push_back(nats.queue_subscribe(
        "variability.v1.>", "ores.variability.service",
        [&nats, base_ctx = ctx, context_extractor](ores::nats::message msg) mutable {
            auto ctx = (context_extractor ?
                context_extractor(msg) : std::nullopt).value_or(base_ctx);
            service::feature_flags_service svc(ctx);
            const auto& subj = msg.subject;

            if (subj.ends_with(".feature-flags.list")) {
                get_feature_flags_response resp;
                try {
                    resp.feature_flags = svc.get_all_feature_flags();
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".feature-flags.save")) {
                if (auto req = decode<save_feature_flag_request>(msg)) {
                    try {
                        svc.save_feature_flag(req->data);
                        reply(nats, msg,
                            save_feature_flag_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_feature_flag_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".feature-flags.delete")) {
                if (auto req = decode<delete_feature_flag_request>(msg)) {
                    try {
                        svc.delete_feature_flag(req->name);
                        reply(nats, msg,
                            delete_feature_flag_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_feature_flag_response{
                            .success = false, .error_message = e.what()});
                    }
                }

            } else if (subj.ends_with(".feature-flags.history")) {
                if (auto req = decode<get_feature_flag_history_request>(msg)) {
                    try {
                        auto history = svc.get_feature_flag_history(req->name);
                        reply(nats, msg, get_feature_flag_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_feature_flag_history_response{
                            .success = false, .message = e.what()});
                    }
                }
            }
        }));

    return subs;
}

}
