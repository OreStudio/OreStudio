/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_TELEMETRY_EXPORT_LOG_EXPORTER_HPP
#define ORES_TELEMETRY_EXPORT_LOG_EXPORTER_HPP

#include "ores.telemetry/domain/log_record.hpp"

namespace ores::telemetry::exp {

/**
 * @brief Interface for exporting telemetry log records.
 *
 * Implementations of this interface handle the actual export of log records
 * to various backends (files, OpenTelemetry collectors, etc.).
 *
 * The interface is designed to be simple and allow for different export
 * strategies: synchronous, batched, or streaming.
 */
class log_exporter {
public:
    virtual ~log_exporter() = default;

    /**
     * @brief Exports a single log record.
     *
     * Implementations may buffer records for batch export or write immediately.
     *
     * @param record The log record to export.
     */
    virtual void export_record(domain::log_record record) = 0;

    /**
     * @brief Flushes any buffered records.
     *
     * Called to ensure all pending records are exported. Implementations
     * that don't buffer may implement this as a no-op.
     */
    virtual void flush() = 0;

    /**
     * @brief Shuts down the exporter.
     *
     * Called during application shutdown. Implementations should flush
     * any pending records and release resources.
     */
    virtual void shutdown() = 0;
};

}

#endif
