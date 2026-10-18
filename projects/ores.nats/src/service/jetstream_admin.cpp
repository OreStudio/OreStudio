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
#include "ores.nats/service/jetstream_admin.hpp"
#include "ores.logging/make_logger.hpp"
#include <cstdint>
#include <nats/nats.h>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::nats::service {

namespace {

using namespace ores::logging;

inline std::string_view logger_name = "ores.nats.service.jetstream_admin";

auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/**
 * @brief Whether a stream's filter is already exactly the one asked for.
 *
 * The subjects are the contract that decides which publishes a stream holds,
 * so an order difference counts as a difference.
 */
bool same_subjects(const jsStreamConfig* cfg, const std::vector<std::string>& wanted) {
    if (!cfg || cfg->SubjectsLen != static_cast<int>(wanted.size()))
        return false;
    for (int i = 0; i < cfg->SubjectsLen; ++i) {
        if (wanted[static_cast<std::size_t>(i)] != cfg->Subjects[i])
            return false;
    }
    return true;
}

}

jetstream_admin::jetstream_admin(void* js_ctx) noexcept
    : js_ctx_(js_ctx) {}

void jetstream_admin::ensure_stream(std::string_view name,
                                    std::vector<std::string> subjects,
                                    int max_age_days,
                                    stream_retention retention) {

    auto* js = static_cast<jsCtx*>(js_ctx_);
    const std::string name_str(name);

    jsOptions opts;
    jsOptions_Init(&opts);
    auto jerr = jsErrCode(0);

    const int64_t max_age_ns = static_cast<int64_t>(max_age_days) * 24LL * 3600LL * 1'000'000'000LL;
    const jsRetentionPolicy wanted_retention =
        retention == stream_retention::interest ? js_InterestPolicy : js_LimitsPolicy;

    // Check whether the stream already exists.
    jsStreamInfo* info = nullptr;
    natsStatus s = js_GetStreamInfo(&info, js, name_str.c_str(), &opts, &jerr);

    if (s == NATS_OK) {
        // The subject filter is the contract that decides which publishes the
        // stream holds, and a subject renamed anywhere in the tree leaves a
        // stream that no longer covers what its own service publishes: every
        // publish then finds no stream and fails with no responders. The
        // retention policy and the age limit are the other two halves of that
        // contract, and a stream created by an earlier build carries neither.
        // Reconcile all three rather than assume the stream still holds.
        const bool current = info->Config != nullptr && same_subjects(info->Config, subjects) &&
                             info->Config->Retention == wanted_retention &&
                             info->Config->MaxAge == max_age_ns;
        if (current) {
            jsStreamInfo_Destroy(info);
            return;
        }

        std::vector<const char*> updated_ptrs;
        updated_ptrs.reserve(subjects.size());
        for (const auto& sub : subjects)
            updated_ptrs.push_back(sub.c_str());

        // Copy the stream's own configuration and replace only what differs, so
        // a reconciliation changes nothing else about the stream.
        jsStreamConfig cfg = *info->Config;
        cfg.Subjects = updated_ptrs.data();
        cfg.SubjectsLen = static_cast<int>(updated_ptrs.size());
        cfg.Retention = wanted_retention;
        cfg.MaxAge = max_age_ns;

        jsStreamInfo* updated = nullptr;
        jerr = jsErrCode(0);
        s = js_UpdateStream(&updated, js, &cfg, &opts, &jerr);
        if (updated)
            jsStreamInfo_Destroy(updated);
        jsStreamInfo_Destroy(info);

        if (s != NATS_OK)
            throw std::runtime_error(std::string("ensure_stream '") + name_str +
                                     "' subject update failed: " + natsStatus_GetText(s) +
                                     " (js err " + std::to_string(static_cast<int>(jerr)) + ")");
        return;
    }

    if (info)
        jsStreamInfo_Destroy(info);

    if (s != NATS_NOT_FOUND && jerr != JSStreamNotFoundErr)
        throw std::runtime_error(std::string("ensure_stream '") + name_str +
                                 "' info check failed: " + natsStatus_GetText(s));

    // Stream does not exist — create it.
    std::vector<const char*> subject_ptrs;
    subject_ptrs.reserve(subjects.size());
    for (const auto& sub : subjects)
        subject_ptrs.push_back(sub.c_str());

    jsStreamConfig cfg;
    jsStreamConfig_Init(&cfg);
    cfg.Name = name_str.c_str();
    cfg.Subjects = subject_ptrs.data();
    cfg.SubjectsLen = static_cast<int>(subject_ptrs.size());
    cfg.Storage = js_FileStorage;
    cfg.MaxAge = max_age_ns;
    cfg.Retention = wanted_retention;

    info = nullptr;
    jerr = jsErrCode(0);
    s = js_AddStream(&info, js, &cfg, &opts, &jerr);
    if (info)
        jsStreamInfo_Destroy(info);

    if (s == NATS_OK)
        return; // created

    // Another instance may have raced us to create it — treat as success.
    if (jerr == JSStreamNameExistErr)
        return;

    throw std::runtime_error(std::string("ensure_stream '") + name_str +
                             "' create failed: " + natsStatus_GetText(s) + " (js err " +
                             std::to_string(static_cast<int>(jerr)) + ")");
}

void jetstream_admin::prune_consumers(std::string_view name, std::string_view keep) {
    auto* js = static_cast<jsCtx*>(js_ctx_);
    const std::string name_str(name);
    const std::string keep_str(keep);

    jsOptions opts;
    jsOptions_Init(&opts);
    auto jerr = jsErrCode(0);

    jsConsumerNamesList* list = nullptr;
    const natsStatus s = js_ConsumerNames(&list, js, name_str.c_str(), &opts, &jerr);
    if (s != NATS_OK) {
        if (list)
            jsConsumerNamesList_Destroy(list);
        // Not fatal: the consumer that serves this stream is not among the ones
        // this call would have deleted.
        BOOST_LOG_SEV(lg(), warn) << "Could not list the consumers of '" << name_str
                                  << "' to prune them: " << natsStatus_GetText(s);
        return;
    }

    std::vector<std::string> stale;
    if (list != nullptr) {
        for (int i = 0; i < list->Count; ++i) {
            const char* consumer = list->List[i];
            if (consumer != nullptr && keep_str != consumer)
                stale.emplace_back(consumer);
        }
        jsConsumerNamesList_Destroy(list);
    }

    int pruned = 0;
    for (const auto& consumer : stale) {
        jsOptions del_opts;
        jsOptions_Init(&del_opts);
        jerr = jsErrCode(0);
        const natsStatus ds =
            js_DeleteConsumer(js, name_str.c_str(), consumer.c_str(), &del_opts, &jerr);
        if (ds != NATS_OK) {
            BOOST_LOG_SEV(lg(), warn) << "Could not prune consumer '" << consumer << "' on '"
                                      << name_str << "': " << natsStatus_GetText(ds);
            continue;
        }
        ++pruned;
    }

    if (pruned > 0)
        BOOST_LOG_SEV(lg(), info) << "Pruned " << pruned << " consumer(s) on '" << name_str
                                  << "' that the service no longer declares";
}

std::optional<consumer_status> jetstream_admin::consumer_state(std::string_view stream,
                                                               std::string_view consumer) {
    auto* js = static_cast<jsCtx*>(js_ctx_);
    const std::string stream_str(stream);
    const std::string consumer_str(consumer);

    jsOptions opts;
    jsOptions_Init(&opts);
    auto jerr = jsErrCode(0);

    jsConsumerInfo* info = nullptr;
    const natsStatus s =
        js_GetConsumerInfo(&info, js, stream_str.c_str(), consumer_str.c_str(), &opts, &jerr);
    if (s != NATS_OK || info == nullptr) {
        if (info != nullptr)
            jsConsumerInfo_Destroy(info);
        if (s == NATS_NOT_FOUND || jerr == JSConsumerNotFoundErr)
            return std::nullopt;
        throw std::runtime_error("Could not read consumer '" + consumer_str + "' on '" +
                                 stream_str + "': " + natsStatus_GetText(s));
    }

    consumer_status status;
    status.created_ns = info->Created;
    status.delivered = info->Delivered.Stream;
    status.ack_floor_stream = info->AckFloor.Stream;
    status.num_ack_pending = info->NumAckPending;
    status.num_redelivered = info->NumRedelivered;
    status.num_pending = info->NumPending;
    status.push_bound = info->PushBound;
    if (info->Config != nullptr) {
        status.max_deliver = info->Config->MaxDeliver;
        status.ack_wait_ns = info->Config->AckWait;
    }
    jsConsumerInfo_Destroy(info);
    return status;
}

void jetstream_admin::delete_stream(std::string_view name) {
    auto* js = static_cast<jsCtx*>(js_ctx_);
    const std::string name_str(name);

    jsOptions opts;
    jsOptions_Init(&opts);
    auto jerr = jsErrCode(0);

    const natsStatus s = js_DeleteStream(js, name_str.c_str(), &opts, &jerr);
    if (s == NATS_OK || s == NATS_NOT_FOUND || jerr == JSStreamNotFoundErr)
        return;
    throw std::runtime_error("Could not delete stream '" + name_str +
                             "': " + natsStatus_GetText(s));
}

}
