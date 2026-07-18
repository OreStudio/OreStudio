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
#ifndef ORES_QT_CRM_CROSS_RATES_MATRIX_MDI_WINDOW_HPP
#define ORES_QT_CRM_CROSS_RATES_MATRIX_MDI_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.client/crm_client.hpp"
#include "ores.marketdata.client/presentation/crm_rate_display_service.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.refdata.client/service/cache/currency_pair_convention_cache.hpp"
#include <QComboBox>
#include <QLabel>
#include <QPushButton>
#include <QSize>
#include <QTableWidget>
#include <QTimer>
#include <QToolBar>
#include <QWidget>
#include <deque>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::qt {

class ImageCache;
class CrmRateSparklineWidget;

/**
 * @brief Read-only NxN cross-rates matrix for one named CRM (or "All"),
 * for the current party.
 *
 * The named CRM is fixed at construction (chosen up front via the
 * controller's picker, see CrmCrossRatesMatrixController::showMatrix) --
 * this window never switches CRM after opening; comparing two CRMs means
 * opening two windows, one per name. Sourced from repeated
 * `marketdata.v1.crm.rates` requests -- the manual Reload button, and a
 * client-side polling timer driven by the footer's "Update Interval"
 * combo (defaults to 5s, matching the reference mockup; "No
 * auto-refresh" stops it). This is still no server-side broadcast/push
 * and no NATS subscription: the story's "never broadcast the derived
 * set" decision is about the server never pushing ticks to every
 * subscriber, not about whether a client automates its own on-demand
 * pulls -- see the story's `* Decisions` for the full reasoning.
 * Rows/columns are the currencies that actually appear in the response,
 * not a fixed/curated list -- see the reference mockup at
 * doc/analysis/gemini_cross_rates_matrix.png for the general shape
 * (grid, per-cell rate, dashed diagonal, currency-count footer).
 */
class CrmCrossRatesMatrixMdiWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.crm_cross_rates_matrix_mdi_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /// crmName empty selects "All" (every enabled CRM for the party).
    explicit CrmCrossRatesMatrixMdiWindow(ClientManager* clientManager,
                                          ImageCache* imageCache,
                                          QString crmName,
                                          QWidget* parent = nullptr);
    ~CrmCrossRatesMatrixMdiWindow() override = default;

    QSize sizeHint() const override {
        return QSize{760, 520};
    }

public slots:
    void reload();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private slots:
    void onCellSelected(int row, int column);
    void onRefreshIntervalChanged();
    void onHideEmptyToggled(bool checked);
    void onShowReciprocalToggled(bool checked);
    void exportToCsv();
    void exportToOre();

private:
    void setupUi();
    void setupToolbar();
    void updateOverviewPanel(const std::string& base, const std::string& quote);

    /// Descriptive slug for export default filenames -- CRM name, Base
    /// Currency filter, and which of Show Reciprocal/Hide Empty are on --
    /// so two exports taken under different filters (e.g. reciprocal vs.
    /// not) never look identical in a file listing/downloads folder.
    QString exportFileNameSlug() const;

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    /// Fixed for this window's lifetime -- empty means "All".
    QString crmName_;
    /// QSettings group this window's "Hide Empty" preference is stored
    /// under -- one per named CRM, matching the geometry key the
    /// controller assigns, so each named CRM's window remembers its own
    /// preference independently.
    QString settingsGroup_;

    QToolBar* toolbar_;
    QComboBox* baseCurrencyCombo_;
    QComboBox* refreshIntervalCombo_;
    QAction* reloadAction_;
    QAction* exportCsvAction_;
    QAction* exportOreAction_;
    QPushButton* hideEmptyButton_;
    QPushButton* showReciprocalButton_;
    QTimer* autoRefreshTimer_;
    QTableWidget* table_;
    QLabel* footerLabel_;

    // "FX Pair Overview" side panel for the currently selected cell.
    QLabel* overviewPairLabel_;
    QLabel* overviewRateLabel_;
    CrmRateSparklineWidget* overviewSparkline_;

    /// Currencies currently shown as row headers, indexed by the grid's
    /// row position. Kept separate from the column headers below: they
    /// diverge whenever the Base Currency filter narrows rows to a
    /// single entry, or Hide Empty removes different currencies from
    /// each axis (e.g. a USD-pivot-star tier like exotics/scandies).
    std::vector<std::string> displayedRowCurrencies_;

    /// Currencies currently shown as column headers, indexed by the
    /// grid's column position -- see displayedRowCurrencies_.
    std::vector<std::string> displayedColumnCurrencies_;

    /// Rolling per-session rate history per (base, quote), one point per
    /// reload -- feeds the overview panel's sparkline. Capped so an
    /// all-day session doesn't grow this unbounded.
    std::map<std::pair<std::string, std::string>, std::deque<double>> rateHistory_;

    /// Exactly the cells actually rendered as a real quote/derived rate
    /// in the most recent reload (respecting the current Base
    /// Currency/Hide Empty/Show Reciprocal filters) -- diagonal and dash
    /// ("no data") cells are excluded. Export actions (CSV, ORE market
    /// data) work from this: what's on screen, not a fresh unfiltered
    /// fetch.
    std::vector<ores::marketdata::client::presentation::crm_rate_display_service::row>
        displayedRates_;

    /// Warmed once at construction (this window's own tenant), consulted by
    /// displayService_ on every reload -- see crm_rate_display_service.
    std::shared_ptr<ores::refdata::service::cache::currency_pair_convention_cache> conventionCache_;

    /// The "underlying CRM" -- thin authenticated facade over the
    /// marketdata.v1.crm.rates NATS request. Owned here (not by
    /// displayService_) so both share the same lifetime as this window.
    std::unique_ptr<ores::marketdata::client::crm_client> crmClient_;

    /// Facade over crmClient_ + conventionCache_ + crm_rate_formatter --
    /// reload() makes exactly one rates() call and gets back rows already
    /// formatted (convention-aware rate/%-change/tooltip text), instead of
    /// hand-rolling the NATS request, cache lookup, and formatting itself.
    std::unique_ptr<ores::marketdata::client::presentation::crm_rate_display_service>
        displayService_;
};

}

#endif
