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
#include "ores.cli/app/host.hpp"
#include "ores.cli/app/application.hpp"
#include "ores.cli/config/parser.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.service/service/host_runner_sync.hpp"
#include <boost/exception/diagnostic_information.hpp>
#include <optional>
#include <ostream>

namespace ores::cli::app {

using ores::cli::config::parser;
using ores::service::service::run_host_sync;

int host::execute(const std::vector<std::string>& args,
                  std::ostream& std_output,
                  std::ostream& error_output) {
    using namespace ores::logging;

    /*
     * DB-free commands are intercepted here, before constructing application.
     */
    const auto early_exit = [&std_output](const auto& cfg) -> std::optional<int> {
        if (!cfg.ore_roundtrip.has_value())
            return std::nullopt;

        try {
            const auto& opts = *cfg.ore_roundtrip;
            const auto s = ores::ore::xml::exporter::roundtrip(opts.input_dir, opts.output_dir);
            const int total_trades = s.trades_mapped + s.trades_passthrough;
            const double tps = s.total_ms > 0 ? total_trades * 1000.0 / s.total_ms : 0.0;
            std_output << "XML files found:      " << s.total_xml_files << "\n"
                       << "Skipped:              " << s.skipped << "\n"
                       << "Outputs written:      " << s.output_files_written << "\n"
                       << "Trades mapped:        " << s.trades_mapped << "\n"
                       << "Trades passthrough:   " << s.trades_passthrough << "\n"
                       << "Currency files:       " << s.currency_files << "\n"
                       << "Calendar files:       " << s.calendar_files << "\n"
                       << "Convention files:     " << s.convention_files << "\n"
                       << "Import time (ms):     " << s.import_ms << "\n"
                       << "Export time (ms):     " << s.export_ms << "\n"
                       << "Total time (ms):      " << s.total_ms << "\n"
                       << "Throughput:           " << static_cast<int>(tps) << " trades/s\n";
            return EXIT_SUCCESS;
        } catch (const std::exception& e) {
            const auto* const be(dynamic_cast<const boost::exception* const>(&e));
            if (be != nullptr) {
                using boost::diagnostic_information;
                BOOST_LOG_SEV(lg(), error) << "Roundtrip error: " << diagnostic_information(*be);
            } else {
                BOOST_LOG_SEV(lg(), error) << "Roundtrip error: " << e.what();
            }
            throw;
        }
    };

    const auto run_application = [&std_output](const auto& cfg) {
        ores::cli::app::application app(std_output, cfg.database);
        app.run(cfg);
    };

    return run_host_sync<parser>(args, std_output, error_output, lg(), early_exit, run_application);
}

}
