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
#ifndef ORES_NATS_SERVICE_JETSTREAM_ADMIN_HPP
#define ORES_NATS_SERVICE_JETSTREAM_ADMIN_HPP

#include "ores.nats/export.hpp"
#include <string_view>
#include <vector>

namespace ores::nats::service {

/**
 * @brief JetStream stream provisioning.
 *
 * Wraps the cnats stream-creation function. No cnats types appear in this
 * header: all cnats state is held as an opaque void* internally.
 *
 * Obtain an instance via client::make_admin() after connecting. The admin is
 * lightweight (a single pointer) and can be copied freely. The underlying
 * client must outlive all admin instances derived from it.
 *
 * Every method throws std::runtime_error on failure.
 */
class ORES_NATS_EXPORT jetstream_admin {
public:
    /**
     * @brief Construct from an opaque JetStream context pointer.
     *
     * Only callable from client::make_admin(). The void* is a jsCtx*.
     */
    explicit jetstream_admin(void* js_ctx) noexcept;

    /**
     * @brief Create a durable stream covering @p subjects, or update it if a
     *        stream with @p name already exists.
     *
     * Idempotent: safe to call on every service startup. The stream uses
     * file-backed storage and retains messages for up to @p max_age_days days.
     *
     * @param name          Stream name (NATS naming rules: A-Z a-z 0-9 - _).
     * @param subjects      Subjects the stream captures (fully-qualified,
     *                      including any subject prefix).
     * @param max_age_days  How long messages are retained (default: 7 days).
     */
    void
    ensure_stream(std::string_view name, std::vector<std::string> subjects, int max_age_days = 7);

private:
    // An opaque jsCtx*, not owned.
    void* js_ctx_;
};

}

#endif
