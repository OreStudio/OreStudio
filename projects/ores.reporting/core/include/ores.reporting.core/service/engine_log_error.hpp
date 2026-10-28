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
#ifndef ORES_REPORTING_CORE_SERVICE_ENGINE_LOG_ERROR_HPP
#define ORES_REPORTING_CORE_SERVICE_ENGINE_LOG_ERROR_HPP

#include "ores.reporting.core/export.hpp"
#include <string>

namespace ores::reporting::service {

/**
 * @brief One line of a run's log that says something went wrong.
 *
 * This is the row the service stores against a run. It is filled from the
 * canonical rows the node marshalled, never from the engine's own files: the
 * engine's format belongs to ores.ore and the node alone reads it. A field the
 * node did not state stays empty here.
 */
struct engine_log_error {
    /// The severity the node stated: =error= or =warning=.
    std::string severity;

    /// The group the engine put the message in, such as =Curve= or =Trade=.
    std::string scope;

    /// What the engine said.
    std::string message;

    /// The kind of record the message is about, when it names one: =trade=.
    std::string subject;

    /// The record the message is about, when it names one.
    std::string subject_id;

    /// The thing the error is about in a person's words: the curve that would
    /// not build, or the curve the failed trade needed.
    std::string cause;

    /// The sub-fields other than the identifying one, as the node passed them on.
    std::string detail;

    /// The time the engine stated, as the node passed it on.
    std::string event_time;

    /// The file the row came from.
    std::string source;
};

}

#endif
