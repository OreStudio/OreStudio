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
#include "ores.ore.core/xml/roundtrip.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <chrono>
#include <stdexcept>
#include <string>
#include <variant>

namespace ores::ore::xml {

using namespace ores::logging;

namespace {

constexpr std::string_view logger_name = "ores.ore.xml.roundtrip";

auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

using clock_type = std::chrono::steady_clock;
using millis = std::chrono::milliseconds;

/**
 * @brief Imports one document and exports it back.
 *
 * The only place in the component that calls both the importer and the
 * exporter. It throws when either half fails, so the caller decides what a
 * failure means rather than reading it out of a shared counter.
 */
std::string convert(const std::filesystem::path& file,
                    document_kind kind,
                    roundtrip_summary& summary) {
    switch (kind) {
    case document_kind::portfolio: {
        const auto t0 = clock_type::now();
        auto items = importer::import_portfolio_with_context(file);
        summary.import_ms +=
            std::chrono::duration_cast<millis>(clock_type::now() - t0).count();

        std::vector<trading::messaging::trade_export_item> export_items;
        export_items.reserve(items.size());
        for (const auto& item : items) {
            trading::messaging::trade_export_item export_item;
            export_item.trade = item.trade;
            export_item.instrument = trading::domain::encode_instrument(item.instrument);
            export_item.envelope = item.envelope;
            if (std::holds_alternative<std::monostate>(item.instrument))
                ++summary.trades_passthrough;
            else
                ++summary.trades_mapped;
            export_items.push_back(std::move(export_item));
        }

        const auto t1 = clock_type::now();
        auto xml = exporter::export_portfolio(export_items);
        summary.export_ms +=
            std::chrono::duration_cast<millis>(clock_type::now() - t1).count();
        return xml;
    }
    case document_kind::currency_config: {
        const auto t0 = clock_type::now();
        auto currencies = importer::import_currency_config(file);
        summary.import_ms +=
            std::chrono::duration_cast<millis>(clock_type::now() - t0).count();

        const auto t1 = clock_type::now();
        auto xml = exporter::export_currency_config(currencies);
        summary.export_ms +=
            std::chrono::duration_cast<millis>(clock_type::now() - t1).count();
        ++summary.currency_files;
        return xml;
    }
    case document_kind::calendar_adjustments: {
        const auto t0 = clock_type::now();
        auto adjustments = importer::import_calendar_adjustments(file);
        summary.import_ms +=
            std::chrono::duration_cast<millis>(clock_type::now() - t0).count();

        const auto t1 = clock_type::now();
        auto xml = exporter::export_calendar_adjustments(adjustments);
        summary.export_ms +=
            std::chrono::duration_cast<millis>(clock_type::now() - t1).count();
        ++summary.calendar_files;
        return xml;
    }
    case document_kind::conventions: {
        const auto t0 = clock_type::now();
        auto mc = importer::import_conventions(file);
        summary.import_ms +=
            std::chrono::duration_cast<millis>(clock_type::now() - t0).count();

        const auto t1 = clock_type::now();
        auto xml = exporter::export_conventions(mc);
        summary.export_ms +=
            std::chrono::duration_cast<millis>(clock_type::now() - t1).count();
        ++summary.convention_files;
        return xml;
    }
    }
    throw std::logic_error("unhandled document kind");
}

} // namespace

roundtrip_summary roundtrip(const std::filesystem::path& input_dir,
                            const std::filesystem::path& output_dir) {
    BOOST_LOG_SEV(lg(), debug) << "Starting roundtrip. Input: " << input_dir
                               << " Output: " << output_dir;

    roundtrip_summary summary;
    const auto wall_start = clock_type::now();

    for (const auto& entry : std::filesystem::recursive_directory_iterator(input_dir)) {
        if (!entry.is_regular_file())
            continue;
        if (entry.path().extension() != ".xml")
            continue;

        ++summary.total_xml_files;
        const auto& file = entry.path();
        std::string relative;
        try {
            relative = std::filesystem::relative(file, input_dir).string();
            const auto kind = detect_document_kind(file);
            if (!kind) {
                BOOST_LOG_SEV(lg(), debug)
                    << "Unsupported document, not written: " << relative;
                ++summary.unsupported;
                continue;
            }

            const auto xml = convert(file, *kind, summary);

            const auto out_path = output_dir / relative;
            std::filesystem::create_directories(out_path.parent_path());
            platform::filesystem::file::write_content(out_path, xml);
            ++summary.output_files_written;
            BOOST_LOG_SEV(lg(), trace) << "Written: " << out_path;
        } catch (const std::exception& e) {
            ++summary.failed;
            const auto where = relative.empty() ? file.string() : relative;
            summary.failures.push_back(where + ": " + e.what());
            BOOST_LOG_SEV(lg(), warn) << "Failed: " << where << ": " << e.what();
        }
    }

    summary.total_ms =
        std::chrono::duration_cast<millis>(clock_type::now() - wall_start).count();

    BOOST_LOG_SEV(lg(), debug)
        << "Roundtrip complete."
        << " Total: " << summary.total_xml_files << " Unsupported: " << summary.unsupported
        << " Failed: " << summary.failed
        << " Written: " << summary.output_files_written
        << " Mapped: " << summary.trades_mapped
        << " Passthrough: " << summary.trades_passthrough
        << " Currencies: " << summary.currency_files
        << " Calendars: " << summary.calendar_files
        << " Conventions: " << summary.convention_files << " Import ms: " << summary.import_ms
        << " Export ms: " << summary.export_ms << " Total ms: " << summary.total_ms;

    return summary;
}

}
