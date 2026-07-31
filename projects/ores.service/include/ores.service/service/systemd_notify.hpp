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
#ifndef ORES_SERVICE_SERVICE_SYSTEMD_NOTIFY_HPP
#define ORES_SERVICE_SERVICE_SYSTEMD_NOTIFY_HPP

#include "ores.service/export.hpp"

namespace ores::service {

/**
 * @brief Tells systemd this process is ready, if running under it.
 *
 * Implements the sd_notify(3) wire protocol directly (a single
 * "READY=1" datagram to the AF_UNIX socket named by $NOTIFY_SOCKET)
 * rather than linking libsystemd, since this one message is all any
 * caller needs. A no-op when $NOTIFY_SOCKET is unset -- e.g. running
 * outside systemd entirely, or under podman (its own --sdnotify=healthy
 * uses the container's HEALTHCHECK instead, not this path). Safe to
 * call unconditionally; never throws.
 */
ORES_SERVICE_EXPORT void notify_systemd_ready() noexcept;

}

#endif
