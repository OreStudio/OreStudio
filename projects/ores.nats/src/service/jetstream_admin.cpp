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
#include <cstdint>
#include <nats/nats.h>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::nats::service {

jetstream_admin::jetstream_admin(void* js_ctx) noexcept
    : js_ctx_(js_ctx) {}

void jetstream_admin::ensure_stream(std::string_view name,
                                    std::vector<std::string> subjects,
                                    int max_age_days) {

    auto* js = static_cast<jsCtx*>(js_ctx_);
    const std::string name_str(name);

    jsOptions opts;
    jsOptions_Init(&opts);
    auto jerr = jsErrCode(0);

    // Check whether the stream already exists.
    jsStreamInfo* info = nullptr;
    natsStatus s = js_GetStreamInfo(&info, js, name_str.c_str(), &opts, &jerr);
    if (info)
        jsStreamInfo_Destroy(info);

    if (s == NATS_OK)
        return; // stream exists — nothing to do

    if (s != NATS_NOT_FOUND && jerr != JSStreamNotFoundErr)
        throw std::runtime_error(std::string("ensure_stream '") + name_str +
                                 "' info check failed: " + natsStatus_GetText(s));

    // Stream does not exist — create it.
    std::vector<const char*> subject_ptrs;
    subject_ptrs.reserve(subjects.size());
    for (const auto& sub : subjects)
        subject_ptrs.push_back(sub.c_str());

    const int64_t max_age_ns = static_cast<int64_t>(max_age_days) * 24LL * 3600LL * 1'000'000'000LL;

    jsStreamConfig cfg;
    jsStreamConfig_Init(&cfg);
    cfg.Name = name_str.c_str();
    cfg.Subjects = subject_ptrs.data();
    cfg.SubjectsLen = static_cast<int>(subject_ptrs.size());
    cfg.Storage = js_FileStorage;
    cfg.MaxAge = max_age_ns;

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

}
