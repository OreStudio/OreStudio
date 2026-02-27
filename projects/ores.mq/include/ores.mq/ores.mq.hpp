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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MQ_HPP
#define ORES_MQ_HPP

/**
 * @brief Message queue support for OreStudio, built on top of pgmq.
 *
 * This component provides two layers of message queue functionality:
 *
 * - @b pgmq sub-namespace: a low-level C++23 wrapper around the pgmq
 *   PostgreSQL extension. The API mirrors Npgmq (the .NET pgmq client),
 *   mapping its queue management, send, read, archive, and delete operations
 *   to idiomatic C++23 using sqlgen for database access.
 *
 * - @b domain sub-namespace: a high-level API that hides pgmq internals and
 *   integrates with the ores.comms remoting layer, allowing messages to be
 *   sent and received transparently over the communications infrastructure.
 */
namespace ores::mq {}

#endif
