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
#include <cctype>
#include <fstream>
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

/// How much of a document is read to find its root element.
constexpr std::size_t kPeek = 4096;

std::string read_header(const std::filesystem::path& file) {
    std::ifstream ifs(file, std::ios::binary);
    if (!ifs)
        return {};
    std::string buf(kPeek, '\0');
    ifs.read(buf.data(), static_cast<std::streamsize>(kPeek));
    buf.resize(static_cast<std::size_t>(ifs.gcount()));
    return buf;
}

/**
 * @brief Reads the name of a document's root element.
 *
 * The reader is chosen from the root element alone. Searching the header
 * for a keyword instead would read a curve configuration that names a
 * conventions block in its opening lines as a conventions document, and
 * rewrite it as an empty one: the generated loader accepts any document
 * whose root element is one of the schema's, leaves the unmatched members
 * default-empty, and the export then writes that emptiness out.
 */
std::string read_root_element(const std::filesystem::path& file) {
    const auto header = read_header(file);
    auto at = header.find('<');
    while (at != std::string::npos) {
        if (header.compare(at, 4, "<!--") == 0) {
            const auto end = header.find("-->", at + 4);
            if (end == std::string::npos)
                return {};
            at = header.find('<', end + 3);
            continue;
        }
        if (header.compare(at, 2, "<?") == 0 || header.compare(at, 2, "<!") == 0) {
            const auto end = header.find('>', at + 2);
            if (end == std::string::npos)
                return {};
            at = header.find('<', end + 1);
            continue;
        }
        auto end = at + 1;
        while (end < header.size() && (std::isalpha(static_cast<unsigned char>(header[end])) ||
                                       header[end] == '_' || header[end] == ':'))
            ++end;
        return header.substr(at + 1, end - at - 1);
    }
    return {};
}

std::optional<document_kind> kind_of(const std::string& root) {
    if (root == "Portfolio")
        return document_kind::portfolio;
    if (root == "CurrencyConfig")
        return document_kind::currency_config;
    if (root == "CalendarAdjustments")
        return document_kind::calendar_adjustments;
    if (root == "Conventions")
        return document_kind::conventions;
    return std::nullopt;
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

std::optional<document_kind> detect_document_kind(const std::filesystem::path& path) {
    return kind_of(read_root_element(path));
}

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
