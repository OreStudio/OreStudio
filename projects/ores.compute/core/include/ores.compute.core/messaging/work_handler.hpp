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
#ifndef ORES_COMPUTE_MESSAGING_WORK_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_WORK_HANDLER_HPP

#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/compute_telemetry_repository.hpp"
#include "ores.compute.core/repository/work_ledger_repository.hpp"
#include "ores.compute.core/service/host_service.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/change_reason.hpp"
#include "ores.dq.api/domain/change_reason_codes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <array>
#include <chrono>
#include <cstdint>
#include <map>
#include <rfl/json.hpp>
#include <stdexcept>

namespace ores::compute::messaging {

namespace {
inline auto& work_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.compute.messaging.work_handler");
    return instance;
}

/**
 * @brief Generates a whimsical adjective-animal display name with a
 * hexadecimal discriminator, from a UUID.
 *
 * The first four bytes of the UUID seed the adjective and the animal; the
 * next four bytes are appended as eight hexadecimal digits to separate names
 * that would otherwise collide. Every part comes from the host id alone, so
 * the name is deterministic: the same host id always produces the same name.
 */
inline std::string make_display_name(const boost::uuids::uuid& id) {
    static constexpr std::array adjectives = {
        "amber", "bold",   "brave",   "bumbling", "calm",     "clever",   "daring", "dazzling",
        "eager", "fierce", "frantic", "gentle",   "graceful", "happy",    "humble", "idle",
        "jolly", "kind",   "lively",  "merry",    "nimble",   "outgoing", "plucky", "quiet",
        "rowdy", "rusty",  "swift",   "timid",    "upbeat",   "vibrant",  "witty",  "zany"};
    static constexpr std::array animals = {
        "armadillo", "badger",  "bison",  "capybara", "caracal", "dhole",   "dingo",      "echidna",
        "ermine",    "falcon",  "gecko",  "hamster",  "iguana",  "jackal",  "koala",      "lemur",
        "marmot",    "narwhal", "orca",   "pangolin", "quokka",  "raccoon", "salamander", "tapir",
        "uakari",    "viper",   "walrus", "xerus",    "yak",     "zorilla"};
    static constexpr char hex_digits[] = "0123456789abcdef";
    const std::uint32_t seed = (static_cast<std::uint32_t>(id.data[0]) << 24) |
                               (static_cast<std::uint32_t>(id.data[1]) << 16) |
                               (static_cast<std::uint32_t>(id.data[2]) << 8) |
                               static_cast<std::uint32_t>(id.data[3]);
    const std::uint32_t discriminator = (static_cast<std::uint32_t>(id.data[4]) << 24) |
                                        (static_cast<std::uint32_t>(id.data[5]) << 16) |
                                        (static_cast<std::uint32_t>(id.data[6]) << 8) |
                                        static_cast<std::uint32_t>(id.data[7]);
    std::string name = std::string(adjectives[seed % adjectives.size()]) + "-" +
                       std::string(animals[(seed >> 8) % animals.size()]) + "-";
    name += hex_digits[(discriminator >> 28) & 0x0F];
    name += hex_digits[(discriminator >> 24) & 0x0F];
    name += hex_digits[(discriminator >> 20) & 0x0F];
    name += hex_digits[(discriminator >> 16) & 0x0F];
    name += hex_digits[(discriminator >> 12) & 0x0F];
    name += hex_digits[(discriminator >> 8) & 0x0F];
    name += hex_digits[(discriminator >> 4) & 0x0F];
    name += hex_digits[discriminator & 0x0F];
    return name;
}

} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

class ORES_COMPUTE_CORE_EXPORT work_handler {
public:
    explicit work_handler(ores::database::context ctx)
        : ctx_(std::move(ctx)) {}

    void heartbeat(ores::nats::message msg) {
        BOOST_LOG_SEV(work_handler_lg(), debug) << "Handling " << msg.subject;
        // Heartbeats are unauthenticated fire-and-forget publishes from wrapper
        // nodes — use the service context directly (no JWT required).
        //
        // A heartbeat from a known host writes nothing unless the hardware it
        // reports differs from the row. It used to bump a
        // last-seen column, but that column lives on a bitemporal table, so
        // every heartbeat added a version: five nodes at one heartbeat per
        // thirty seconds grew the host table by fourteen thousand rows a day,
        // all of them differing only in a timestamp. Liveness is a series, and
        // the node samples already carry it.
        if (auto req = decode<heartbeat_message>(msg)) {
            try {
                // A node that holds a job under a lease keeps it by this heartbeat. The ledger is
                // the installation's, so the extension needs no tenant.
                repository::work_ledger_repository().extend_leases(
                    ctx_,
                    boost::lexical_cast<boost::uuids::uuid>(req->host_id),
                    std::chrono::seconds{300});
                service::host_service svc(ctx_);
                auto existing = svc.get_host(boost::lexical_cast<boost::uuids::uuid>(req->host_id));
                if (!existing) {
                    BOOST_LOG_SEV(work_handler_lg(), info)
                        << "Auto-registering new host from heartbeat: " << req->host_id;
                    domain::host h;
                    try {
                        h.id = boost::lexical_cast<boost::uuids::uuid>(req->host_id);
                    } catch (const boost::bad_lexical_cast& e) {
                        BOOST_LOG_SEV(work_handler_lg(), warn)
                            << "Invalid host_id in heartbeat: " << req->host_id << " (" << e.what()
                            << ")";
                        return;
                    }
                    h.external_id = req->host_id;
                    h.display_name = make_display_name(h.id);
                    h.cpu_count = req->cpu_count;
                    h.ram_mb = req->ram_mb;
                    h.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                    h.change_commentary = "Auto-registered on first heartbeat";
                    BOOST_LOG_SEV(work_handler_lg(), info)
                        << "Assigned display name: " << h.display_name;
                    stamp(h, ctx_);
                    svc.save_host(h);
                } else if ((req->cpu_count > 0 && existing->cpu_count != req->cpu_count) ||
                           (req->ram_mb > 0 && existing->ram_mb != req->ram_mb)) {
                    // The row holds the facts the computer gave, and they change only when
                    // the node moves to other hardware, so this writes once per change
                    // and not once per heartbeat.
                    auto h = *existing;
                    h.cpu_count = req->cpu_count;
                    h.ram_mb = req->ram_mb;
                    h.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                    h.change_commentary = "Machine facts reported by the node";
                    stamp(h, ctx_);
                    svc.save_host(h);
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(work_handler_lg(), error)
                    << "Heartbeat error for host " << req->host_id << ": " << e.what();
            }
        } else {
            BOOST_LOG_SEV(work_handler_lg(), warn) << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(work_handler_lg(), debug) << "Completed " << msg.subject;
    }

    void reap(ores::nats::message msg) {
        BOOST_LOG_SEV(work_handler_lg(), debug) << "Handling " << msg.subject;
        // Reaper runs with service context (no per-request JWT needed).
        static constexpr auto stale_threshold = std::chrono::minutes(5);
        try {
            service::result_service result_svc(ctx_);
            repository::compute_telemetry_repository telemetry;
            // A node's liveness is its newest sample; the host row no longer
            // carries a last-seen column.
            std::map<boost::uuids::uuid, std::chrono::system_clock::time_point> last_sample;
            for (const auto& s : telemetry.latest_node_samples(ctx_))
                last_sample[s.host_id] = s.sampled_at;
            // InProgress.
            auto in_progress = result_svc.list_by_state(4);
            int reaped = 0;
            const auto now = std::chrono::system_clock::now();

            for (auto& r : in_progress) {
                if (r.host_id == boost::uuids::uuid{})
                    continue;
                const auto host_id_str = boost::uuids::to_string(r.host_id);
                const auto sample = last_sample.find(r.host_id);
                if (sample == last_sample.end())
                    continue;
                if (now - sample->second <= stale_threshold)
                    continue;

                r.host_id = boost::uuids::uuid{};
                // Unsent - back in the queue.
                r.server_state = 2;
                r.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
                r.change_commentary = "Host went stale; result re-queued";
                stamp(r, ctx_);
                result_svc.save_result(r);
                ++reaped;
                BOOST_LOG_SEV(work_handler_lg(), info)
                    << "Reaped result " << boost::uuids::to_string(r.id) << " from stale host "
                    << host_id_str;
            }

            BOOST_LOG_SEV(work_handler_lg(), info)
                << "Reap complete. Reaped " << reaped << " results.";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(work_handler_lg(), error) << "Reap error: " << e.what();
        }
    }

private:
    ores::database::context ctx_;
};

} // namespace ores::compute::messaging

#endif
