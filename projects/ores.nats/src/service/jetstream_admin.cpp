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

#include <stdexcept>
#include <string>

#include <nats/nats.h>

namespace ores::nats::service {

namespace {

// Convert a nats_time (nanoseconds since epoch) to system_clock::time_point.
std::chrono::system_clock::time_point from_nats_time(int64_t nanos) {
    using namespace std::chrono;
    return system_clock::time_point(
        duration_cast<system_clock::duration>(nanoseconds(nanos)));
}

// Throw a descriptive runtime_error if status is not NATS_OK.
void check(natsStatus s, const char* context) {
    if (s != NATS_OK)
        throw std::runtime_error(
            std::string(context) + ": " + natsStatus_GetText(s));
}

domain::stream_info fill_stream_info(jsStreamInfo* info) {
    domain::stream_info si;
    if (info->Config) {
        if (const char* n = info->Config->Name)
            si.name = n;

        if (info->Config->Subjects) {
            for (int i = 0; i < info->Config->SubjectsLen; ++i) {
                if (info->Config->Subjects[i])
                    si.subjects.emplace_back(info->Config->Subjects[i]);
            }
        }
    }

    si.message_count  = info->State.Msgs;
    si.byte_count     = info->State.Bytes;
    si.consumer_count = static_cast<std::uint64_t>(info->State.Consumers);
    si.first_seq      = info->State.FirstSeq;
    si.last_seq       = info->State.LastSeq;
    si.created_at     = from_nats_time(info->Created);

    if (info->State.FirstSeq > 0)
        si.first_message_at = from_nats_time(info->State.FirstTime);
    if (info->State.LastSeq > 0)
        si.last_message_at  = from_nats_time(info->State.LastTime);

    return si;
}

domain::consumer_info fill_consumer_info(jsConsumerInfo* info) {
    domain::consumer_info ci;
    if (const char* s = info->Stream)
        ci.stream_name = s;
    if (const char* n = info->Name)
        ci.name = n;

    ci.num_pending      = info->NumPending;
    ci.num_ack_pending  = static_cast<std::uint64_t>(info->NumAckPending);
    ci.num_redelivered  = static_cast<std::uint64_t>(info->NumRedelivered);
    ci.delivered_count  = info->Delivered.Consumer;
    ci.created_at       = from_nats_time(info->Created);
    return ci;
}

} // namespace

jetstream_admin::jetstream_admin(void* js_ctx) noexcept
    : js_ctx_(js_ctx) {}

// ---------------------------------------------------------------------------
// Streams
// ---------------------------------------------------------------------------

std::vector<domain::stream_info> jetstream_admin::list_streams() {
    auto* js = static_cast<jsCtx*>(js_ctx_);

    jsStreamInfoList* list = nullptr;
    jsOptions opts;
    jsOptions_Init(&opts);

    auto jerr = jsErrCode(0);
    const natsStatus s = js_Streams(&list, js, &opts, &jerr);
    check(s, "js_Streams");

    std::vector<domain::stream_info> result;
    if (list) {
        result.reserve(static_cast<std::size_t>(list->Count));
        for (int i = 0; i < list->Count; ++i)
            result.push_back(fill_stream_info(list->List[i]));
        jsStreamInfoList_Destroy(list);
    }
    return result;
}

domain::stream_info jetstream_admin::get_stream(std::string_view name) {
    auto* js = static_cast<jsCtx*>(js_ctx_);

    jsStreamInfo* info = nullptr;
    jsOptions opts;
    jsOptions_Init(&opts);

    auto jerr = jsErrCode(0);
    const natsStatus s = js_GetStreamInfo(
        &info, js, std::string(name).c_str(), &opts, &jerr);
    check(s, "js_GetStreamInfo");

    domain::stream_info result = fill_stream_info(info);
    jsStreamInfo_Destroy(info);
    return result;
}

void jetstream_admin::purge_stream(std::string_view name) {
    auto* js = static_cast<jsCtx*>(js_ctx_);

    jsOptions opts;
    jsOptions_Init(&opts);

    auto jerr = jsErrCode(0);
    const natsStatus s = js_PurgeStream(
        js, std::string(name).c_str(), &opts, &jerr);
    check(s, "js_PurgeStream");
}

// ---------------------------------------------------------------------------
// Consumers
// ---------------------------------------------------------------------------

std::vector<domain::consumer_info>
jetstream_admin::list_consumers(std::string_view stream_name) {
    auto* js = static_cast<jsCtx*>(js_ctx_);

    jsConsumerInfoList* list = nullptr;
    jsOptions opts;
    jsOptions_Init(&opts);

    auto jerr = jsErrCode(0);
    const natsStatus s = js_Consumers(
        &list, js, std::string(stream_name).c_str(), &opts, &jerr);
    check(s, "js_Consumers");

    std::vector<domain::consumer_info> result;
    if (list) {
        result.reserve(static_cast<std::size_t>(list->Count));
        for (int i = 0; i < list->Count; ++i)
            result.push_back(fill_consumer_info(list->List[i]));
        jsConsumerInfoList_Destroy(list);
    }
    return result;
}

// ---------------------------------------------------------------------------
// Messages
// ---------------------------------------------------------------------------

domain::stream_message
jetstream_admin::peek_message(std::string_view stream_name,
                              std::uint64_t sequence) {
    auto* js = static_cast<jsCtx*>(js_ctx_);

    natsMsg* msg = nullptr;
    jsOptions opts;
    jsOptions_Init(&opts);

    auto jerr = jsErrCode(0);
    const natsStatus s = js_GetMsg(
        &msg, js, std::string(stream_name).c_str(), sequence, &opts, &jerr);
    check(s, "js_GetMsg");

    domain::stream_message result;
    if (const char* subj = natsMsg_GetSubject(msg))
        result.subject = subj;
    result.sequence  = sequence;
    result.timestamp = from_nats_time(natsMsg_GetTime(msg));

    const int len = natsMsg_GetDataLength(msg);
    if (len > 0) {
        const auto* p = reinterpret_cast<const std::byte*>(natsMsg_GetData(msg));
        result.data.assign(p, p + len);
    }

    natsMsg_Destroy(msg);
    return result;
}

domain::stream_message
jetstream_admin::peek_last_message(std::string_view stream_name,
                                   std::string_view subject) {
    auto* js = static_cast<jsCtx*>(js_ctx_);

    natsMsg* msg = nullptr;
    jsOptions opts;
    jsOptions_Init(&opts);

    auto jerr = jsErrCode(0);
    const natsStatus s = js_GetLastMsg(
        &msg, js,
        std::string(stream_name).c_str(),
        std::string(subject).c_str(),
        &opts, &jerr);
    check(s, "js_GetLastMsg");

    domain::stream_message result;
    if (const char* subj = natsMsg_GetSubject(msg))
        result.subject = subj;
    result.sequence  = natsMsg_GetSequence(msg);
    result.timestamp = from_nats_time(natsMsg_GetTime(msg));

    const int len = natsMsg_GetDataLength(msg);
    if (len > 0) {
        const auto* p = reinterpret_cast<const std::byte*>(natsMsg_GetData(msg));
        result.data.assign(p, p + len);
    }

    natsMsg_Destroy(msg);
    return result;
}

void jetstream_admin::delete_message(std::string_view stream_name,
                                     std::uint64_t sequence) {
    auto* js = static_cast<jsCtx*>(js_ctx_);

    jsOptions opts;
    jsOptions_Init(&opts);

    auto jerr = jsErrCode(0);
    const natsStatus s = js_DeleteMsg(
        js, std::string(stream_name).c_str(), sequence, &opts, &jerr);
    check(s, "js_DeleteMsg");
}

void jetstream_admin::publish(std::string_view subject,
                              std::string_view payload) {
    auto* js = static_cast<jsCtx*>(js_ctx_);

    auto jerr = jsErrCode(0);
    const natsStatus s = js_Publish(
        nullptr, js,
        std::string(subject).c_str(),
        payload.data(),
        static_cast<int>(payload.size()),
        nullptr, &jerr);
    check(s, "js_Publish");
}

}
