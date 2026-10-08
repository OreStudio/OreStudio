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
#ifndef ORES_INBOX_SERVICE_APP_APPROVAL_WINDOWS_HPP
#define ORES_INBOX_SERVICE_APP_APPROVAL_WINDOWS_HPP

#include "ores.nats/service/nats_client.hpp"
#include <chrono>

namespace ores::inbox::service::app {

/**
 * @brief How long an answered request stays in the queue's answered tail.
 *
 * The component declares that an answered request stays in view; the
 * installation says how long, as the variability system setting
 * =inbox.approval_queue.answered_window_seconds=. A window of zero answers no
 * tail, which is what an installation that cannot read the setting gets.
 */
std::chrono::seconds answered_window_seconds(ores::nats::service::nats_client& svc_nats);

/**
 * @brief How far ahead of a deadline the deciders are warned.
 *
 * The other half of the deadline, and the installation's to set: the
 * variability system setting
 * =inbox.approval_expiry.reminder_window_seconds=. A window of zero warns
 * nobody, which is what an installation that cannot read the setting gets.
 */
std::chrono::seconds reminder_window_seconds(ores::nats::service::nats_client& svc_nats);

}

#endif
