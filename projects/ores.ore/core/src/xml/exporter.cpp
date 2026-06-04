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
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/domain/bond_instrument_mapper.hpp"
#include "ores.ore.core/domain/calendar_adjustment_mapper.hpp"
#include "ores.ore.core/domain/commodity_instrument_mapper.hpp"
#include "ores.ore.core/domain/composite_instrument_mapper.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/credit_instrument_mapper.hpp"
#include "ores.ore.core/domain/currency_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/equity_instrument_mapper.hpp"
#include "ores.ore.core/domain/fx_instrument_mapper.hpp"
#include "ores.ore.core/domain/scripted_instrument_mapper.hpp"
#include "ores.ore.core/domain/swap_instrument_mapper.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <chrono>
#include <fstream>

namespace ores::ore::xml {

using refdata::domain::currency;
using namespace ores::logging;
using namespace ores::ore::domain;
using trading::domain::swap_instrument_data;
using trading::domain::fx_instrument_variant;
using trading::domain::bond_instrument;
using trading::domain::credit_instrument;
using trading::domain::equity_instrument_variant;
using trading::domain::commodity_instrument;
using trading::domain::composite_instrument_data;
using trading::domain::scripted_instrument;

namespace {

std::string read_header(const std::filesystem::path& file) {
    constexpr std::size_t kPeek = 4096;
    std::ifstream ifs(file, std::ios::binary);
    if (!ifs)
        return {};
    std::string buf(kPeek, '\0');
    ifs.read(buf.data(), static_cast<std::streamsize>(kPeek));
    buf.resize(static_cast<std::size_t>(ifs.gcount()));
    return buf;
}

void fill_envelope(domain::trade& t, const trading::domain::trade& src) {
    static_cast<std::string&>(t.id) = src.identity.external_id;
    if (!src.classification.netting_set_id.empty()) {
        domain::_NettingSetId_t nsid;
        static_cast<std::string&>(nsid) = src.classification.netting_set_id;
        domain::nettingSetGroup_group_t nsg;
        nsg.NettingSetId = nsid;
        domain::envelope env;
        env.nettingSetGroup = nsg;
        t.Envelope = env;
    }
}

} // namespace

std::string exporter::export_currency_config(const std::vector<currency>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Exporting currency config. Total: " << v.size();
    const auto mapped = domain::currency_mapper::map(v);
    return domain::save_data(mapped);
}

std::string
exporter::export_calendar_adjustments(const std::vector<refdata::domain::calendar_adjustment>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Exporting " << v.size() << " calendar adjustments.";
    const auto xsd = domain::calendar_adjustment_mapper::reverse(v);
    return domain::save_data(xsd);
}

std::string exporter::export_conventions(const domain::mapped_conventions& mc) {
    BOOST_LOG_SEV(lg(), debug) << "Exporting conventions.";
    const auto xsd = domain::conventions_mapper::reverse(mc);
    return domain::save_data(xsd);
}

std::string
exporter::export_portfolio(const std::vector<trading::messaging::trade_export_item>& items) {
    BOOST_LOG_SEV(lg(), debug) << "Started portfolio export. Items: " << items.size();

    domain::portfolio p;
    for (const auto& item : items) {
        const auto& tr = item.trade;
        const auto& tt = tr.classification.trade_type;

        std::visit(
            [&](const auto& r) {
                using T = std::decay_t<decltype(r)>;
                domain::trade xsd_t;

                if constexpr (std::is_same_v<T, std::monostate>) {
                    BOOST_LOG_SEV(lg(), debug)
                        << "Skipping unmapped trade: " << tr.identity.external_id;
                    return;
                } else if constexpr (std::is_same_v<T, swap_instrument_data>) {
                    if (tt == "Swap" || tt == "CrossCurrencySwap")
                        xsd_t = swap_instrument_mapper::reverse_swap(
                            std::get<trading::domain::vanilla_swap_instrument>(r.instrument),
                            r.legs);
                    else if (tt == "ForwardRateAgreement")
                        xsd_t = swap_instrument_mapper::reverse_fra(
                            std::get<trading::domain::fra_instrument>(r.instrument), r.legs);
                    else if (tt == "CapFloor")
                        xsd_t = swap_instrument_mapper::reverse_capfloor(
                            std::get<trading::domain::cap_floor_instrument>(r.instrument), r.legs);
                    else if (tt == "Swaption")
                        xsd_t = swap_instrument_mapper::reverse_swaption(
                            std::get<trading::domain::swaption_instrument>(r.instrument), r.legs);
                    else if (tt == "CallableSwap")
                        xsd_t = swap_instrument_mapper::reverse_callable_swap(
                            std::get<trading::domain::callable_swap_instrument>(r.instrument),
                            r.legs);
                    else {
                        BOOST_LOG_SEV(lg(), debug) << "No reverse mapper for swap type: " << tt;
                        return;
                    }
                } else if constexpr (std::is_same_v<T, fx_instrument_variant>) {
                    // FX reverse mappers are per-type; routing by trade_type_code
                    // mirrors trade_mapper::map_fx_instrument. Keep in sync.
                    bool matched = false;
                    std::visit(
                        [&](const auto& instr) {
                            using I = std::decay_t<decltype(instr)>;
                            using namespace trading::domain;
                            if constexpr (std::is_same_v<I, fx_forward_instrument>) {
                                if (tt == "FxForward") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_forward(instr);
                                    matched = true;
                                } else if (tt == "FxSwap") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_swap(instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I, fx_vanilla_option_instrument>) {
                                xsd_t = fx_instrument_mapper::reverse_fx_option(instr);
                                matched = true;
                            } else if constexpr (std::is_same_v<I, fx_barrier_option_instrument>) {
                                if (tt == "FxBarrierOption") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_barrier_option(instr);
                                    matched = true;
                                } else if (tt == "FxGenericBarrierOption") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_generic_barrier_option(
                                        instr);
                                    matched = true;
                                } else if (tt == "FxDoubleBarrierOption") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_double_barrier_option(
                                        instr);
                                    matched = true;
                                } else if (tt == "FxEuropeanBarrierOption") {
                                    xsd_t =
                                        fx_instrument_mapper::reverse_fx_european_barrier_option(
                                            instr);
                                    matched = true;
                                } else if (tt == "FxKIKOBarrierOption") {
                                    xsd_t =
                                        fx_instrument_mapper::reverse_fx_kiko_barrier_option(instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I, fx_digital_option_instrument>) {
                                if (tt == "FxDigitalOption") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_digital_option(instr);
                                    matched = true;
                                } else if (tt == "FxDigitalBarrierOption") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_digital_barrier_option(
                                        instr);
                                    matched = true;
                                } else if (tt == "FxTouchOption" || tt == "FxDoubleTouchOption") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_touch_option(instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I, fx_asian_forward_instrument>) {
                                if (tt == "FxAverageForward") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_average_forward(instr);
                                    matched = true;
                                } else if (tt == "FxTaRF") {
                                    xsd_t = fx_instrument_mapper::reverse_fx_tarf(instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I, fx_accumulator_instrument>) {
                                xsd_t = fx_instrument_mapper::reverse_fx_accumulator(instr);
                                matched = true;
                            } else if constexpr (std::is_same_v<I, fx_variance_swap_instrument>) {
                                xsd_t = fx_instrument_mapper::reverse_fx_variance_swap(instr);
                                matched = true;
                            }
                        },
                        r);
                    if (!matched) {
                        BOOST_LOG_SEV(lg(), debug) << "No reverse mapper for FX type: " << tt;
                        return;
                    }
                } else if constexpr (std::is_same_v<T, bond_instrument>) {
                    if (tt == "Bond")
                        xsd_t = bond_instrument_mapper::reverse_bond(r);
                    else if (tt == "ForwardBond")
                        xsd_t = bond_instrument_mapper::reverse_forward_bond(r);
                    else if (tt == "CallableBond")
                        xsd_t = bond_instrument_mapper::reverse_callable_bond(r);
                    else if (tt == "ConvertibleBond")
                        xsd_t = bond_instrument_mapper::reverse_convertible_bond(r);
                    else if (tt == "BondOption")
                        xsd_t = bond_instrument_mapper::reverse_bond_option(r);
                    else if (tt == "BondTRS")
                        xsd_t = bond_instrument_mapper::reverse_bond_trs(r);
                    else if (tt == "BondRepo")
                        xsd_t = bond_instrument_mapper::reverse_bond_repo(r);
                    else {
                        BOOST_LOG_SEV(lg(), debug) << "No reverse mapper for bond type: " << tt;
                        return;
                    }
                } else if constexpr (std::is_same_v<T, credit_instrument>) {
                    if (tt == "CreditDefaultSwap")
                        xsd_t = credit_instrument_mapper::reverse_cds(r);
                    else if (tt == "IndexCreditDefaultSwap")
                        xsd_t = credit_instrument_mapper::reverse_index_cds(r);
                    else if (tt == "IndexCreditDefaultSwapOption")
                        xsd_t = credit_instrument_mapper::reverse_index_cds_option(r);
                    else if (tt == "CreditLinkedSwap")
                        xsd_t = credit_instrument_mapper::reverse_credit_linked_swap(r);
                    else if (tt == "SyntheticCDO")
                        xsd_t = credit_instrument_mapper::reverse_synthetic_cdo(r);
                    else if (tt == "RiskParticipationAgreement")
                        xsd_t = credit_instrument_mapper::reverse_rpa(r);
                    else {
                        BOOST_LOG_SEV(lg(), debug) << "No reverse mapper for credit type: " << tt;
                        return;
                    }
                } else if constexpr (std::is_same_v<T, equity_instrument_variant>) {
                    // Equity reverse mappers are per-type; routing by trade_type_code
                    // mirrors trade_mapper::map_equity_instrument. Keep in sync.
                    bool matched = false;
                    std::visit(
                        [&](const auto& instr) {
                            using I = std::decay_t<decltype(instr)>;
                            using namespace trading::domain;
                            if constexpr (std::is_same_v<I, equity_option_instrument>) {
                                if (tt == "EquityOption") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_option(instr);
                                    matched = true;
                                } else if (tt == "EquityCliquetOption") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_cliquet_option(
                                        instr);
                                    matched = true;
                                } else if (tt == "EquityOutperformanceOption") {
                                    xsd_t = equity_instrument_mapper::
                                        reverse_equity_outperformance_option(instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I, equity_forward_instrument>) {
                                if (tt == "EquityForward") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_forward(instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I, equity_swap_instrument>) {
                                if (tt == "EquitySwap") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_swap(instr);
                                    matched = true;
                                } else if (tt == "EquityWorstOfBasketSwap") {
                                    xsd_t = equity_instrument_mapper::
                                        reverse_equity_worst_of_basket_swap(instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I,
                                                                equity_variance_swap_instrument>) {
                                if (tt == "EquityVarianceSwap") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_variance_swap(
                                        instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I,
                                                                equity_barrier_option_instrument>) {
                                if (tt == "EquityBarrierOption") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_barrier_option(
                                        instr);
                                    matched = true;
                                } else if (tt == "EquityDoubleBarrierOption") {
                                    xsd_t = equity_instrument_mapper::
                                        reverse_equity_double_barrier_option(instr);
                                    matched = true;
                                } else if (tt == "EquityEuropeanBarrierOption") {
                                    xsd_t = equity_instrument_mapper::
                                        reverse_equity_european_barrier_option(instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I,
                                                                equity_asian_option_instrument>) {
                                if (tt == "EquityAsianOption") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_asian_option(
                                        instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I,
                                                                equity_digital_option_instrument>) {
                                if (tt == "EquityDigitalOption") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_digital_option(
                                        instr);
                                    matched = true;
                                } else if (tt == "EquityTouchOption") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_touch_option(
                                        instr);
                                    matched = true;
                                }
                            } else if constexpr (std::is_same_v<I, equity_accumulator_instrument>) {
                                if (tt == "EquityAccumulator") {
                                    xsd_t =
                                        equity_instrument_mapper::reverse_equity_accumulator(instr);
                                    matched = true;
                                } else if (tt == "EquityTaRF") {
                                    xsd_t = equity_instrument_mapper::reverse_equity_tarf(instr);
                                    matched = true;
                                }
                            }
                        },
                        r);
                    if (!matched) {
                        BOOST_LOG_SEV(lg(), debug) << "No reverse mapper for equity type: " << tt;
                        return;
                    }
                } else if constexpr (std::is_same_v<T, commodity_instrument>) {
                    if (tt == "CommodityForward")
                        xsd_t = commodity_instrument_mapper::reverse_commodity_forward(r);
                    else if (tt == "CommodityOption")
                        xsd_t = commodity_instrument_mapper::reverse_commodity_option(r);
                    else if (tt == "CommoditySwap")
                        xsd_t = commodity_instrument_mapper::reverse_commodity_swap(r);
                    else if (tt == "CommoditySwaption")
                        xsd_t = commodity_instrument_mapper::reverse_commodity_swaption(r);
                    else if (tt == "CommodityVarianceSwap")
                        xsd_t = commodity_instrument_mapper::reverse_commodity_variance_swap(r);
                    else if (tt == "CommodityAveragePriceOption")
                        xsd_t = commodity_instrument_mapper::reverse_commodity_apo(r);
                    else if (tt == "CommodityOptionStrip")
                        xsd_t = commodity_instrument_mapper::reverse_commodity_option_strip(r);
                    else {
                        BOOST_LOG_SEV(lg(), debug)
                            << "No reverse mapper for commodity type: " << tt;
                        return;
                    }
                } else if constexpr (std::is_same_v<T, scripted_instrument>) {
                    if (tt == "ScriptedTrade")
                        xsd_t = scripted_instrument_mapper::reverse_scripted_trade(r);
                    else if (tt == "DoubleDigitalOption")
                        xsd_t = scripted_instrument_mapper::reverse_double_digital_option(r);
                    else if (tt == "PerformanceOption_01")
                        xsd_t = scripted_instrument_mapper::reverse_performance_option_01(r);
                    else if (tt == "KnockOutSwap")
                        xsd_t = scripted_instrument_mapper::reverse_knock_out_swap(r);
                    else {
                        BOOST_LOG_SEV(lg(), debug) << "No reverse mapper for scripted type: " << tt;
                        return;
                    }
                } else if constexpr (std::is_same_v<T, composite_instrument_data>) {
                    if (tt == "CompositeTrade")
                        xsd_t = composite_instrument_mapper::reverse_composite_trade(r.instrument);
                    else if (tt == "MultiLegOption")
                        xsd_t = composite_instrument_mapper::reverse_multi_leg_option(r.instrument);
                    else if (tt == "TotalReturnSwap")
                        xsd_t =
                            composite_instrument_mapper::reverse_total_return_swap(r.instrument);
                    else if (tt == "ContractForDifference")
                        xsd_t = composite_instrument_mapper::reverse_contract_for_difference(
                            r.instrument);
                    else {
                        BOOST_LOG_SEV(lg(), debug)
                            << "No reverse mapper for composite type: " << tt;
                        return;
                    }
                }

                fill_envelope(xsd_t, tr);
                p.Trade.push_back(std::move(xsd_t));
            },
            item.instrument);
    }

    const std::string result = domain::save_data(p);
    BOOST_LOG_SEV(lg(), debug) << "Finished portfolio export. Trades: " << p.Trade.size();
    return result;
}

roundtrip_summary exporter::roundtrip(const std::filesystem::path& input_dir,
                                      const std::filesystem::path& output_dir) {
    BOOST_LOG_SEV(lg(), debug) << "Starting roundtrip. Input: " << input_dir
                               << " Output: " << output_dir;

    using clock = std::chrono::steady_clock;
    using ms = std::chrono::milliseconds;

    roundtrip_summary summary;
    namespace fs = std::filesystem;

    const auto wall_start = clock::now();

    for (const auto& entry : fs::recursive_directory_iterator(input_dir)) {
        if (!entry.is_regular_file())
            continue;
        if (entry.path().extension() != ".xml")
            continue;

        ++summary.total_xml_files;
        const auto& file = entry.path();
        BOOST_LOG_SEV(lg(), trace) << "Processing: " << file;

        const auto hdr = read_header(file);
        const bool is_portfolio = hdr.find("<Portfolio>") != std::string::npos;
        const bool is_currency = !is_portfolio && hdr.find("<CurrencyConfig>") != std::string::npos;
        const bool is_calendar =
            !is_portfolio && !is_currency && hdr.find("<CalendarAdjustments>") != std::string::npos;
        const bool is_conventions = !is_portfolio && !is_currency && !is_calendar &&
                                    hdr.find("<Conventions>") != std::string::npos;

        if (!is_portfolio && !is_currency && !is_calendar && !is_conventions) {
            BOOST_LOG_SEV(lg(), debug) << "Skipping unrecognised XML: " << file.filename();
            ++summary.skipped;
            continue;
        }

        std::string xml;
        try {
            if (is_portfolio) {
                const auto t0 = clock::now();
                auto import_items = importer::import_portfolio_with_context(file);
                summary.import_ms += std::chrono::duration_cast<ms>(clock::now() - t0).count();

                std::vector<trading::messaging::trade_export_item> export_items;
                export_items.reserve(import_items.size());
                for (const auto& item : import_items) {
                    trading::messaging::trade_export_item ei;
                    ei.trade = item.trade;
                    ei.instrument = item.instrument;
                    if (std::holds_alternative<std::monostate>(item.instrument))
                        ++summary.trades_passthrough;
                    else
                        ++summary.trades_mapped;
                    export_items.push_back(std::move(ei));
                }

                const auto t1 = clock::now();
                xml = export_portfolio(export_items);
                summary.export_ms += std::chrono::duration_cast<ms>(clock::now() - t1).count();

            } else if (is_currency) {
                const auto t0 = clock::now();
                auto currencies = importer::import_currency_config(file);
                summary.import_ms += std::chrono::duration_cast<ms>(clock::now() - t0).count();

                const auto t1 = clock::now();
                xml = export_currency_config(currencies);
                summary.export_ms += std::chrono::duration_cast<ms>(clock::now() - t1).count();
                ++summary.currency_files;

            } else if (is_calendar) {
                const auto t0 = clock::now();
                auto adjustments = importer::import_calendar_adjustments(file);
                summary.import_ms += std::chrono::duration_cast<ms>(clock::now() - t0).count();

                const auto t1 = clock::now();
                xml = export_calendar_adjustments(adjustments);
                summary.export_ms += std::chrono::duration_cast<ms>(clock::now() - t1).count();
                ++summary.calendar_files;

            } else { // is_conventions
                const auto t0 = clock::now();
                auto mc = importer::import_conventions(file);
                summary.import_ms += std::chrono::duration_cast<ms>(clock::now() - t0).count();

                const auto t1 = clock::now();
                xml = export_conventions(mc);
                summary.export_ms += std::chrono::duration_cast<ms>(clock::now() - t1).count();
                ++summary.convention_files;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), debug) << "Skipping " << file.filename() << ": " << e.what();
            ++summary.skipped;
            continue;
        }

        const auto out_path = output_dir / fs::relative(file, input_dir);
        try {
            fs::create_directories(out_path.parent_path());
            platform::filesystem::file::write_content(out_path, xml);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to write " << out_path << ": " << e.what();
            ++summary.skipped;
            continue;
        }
        ++summary.output_files_written;
        BOOST_LOG_SEV(lg(), trace) << "Written: " << out_path;
    }

    summary.total_ms = std::chrono::duration_cast<ms>(clock::now() - wall_start).count();

    BOOST_LOG_SEV(lg(), debug) << "Roundtrip complete."
                               << " Total: " << summary.total_xml_files
                               << " Skipped: " << summary.skipped
                               << " Written: " << summary.output_files_written
                               << " Mapped: " << summary.trades_mapped
                               << " Passthrough: " << summary.trades_passthrough
                               << " Currencies: " << summary.currency_files
                               << " Calendars: " << summary.calendar_files
                               << " Conventions: " << summary.convention_files
                               << " Import ms: " << summary.import_ms
                               << " Export ms: " << summary.export_ms
                               << " Total ms: " << summary.total_ms;

    return summary;
}

}
