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
#include "ores.qt/ClientManager.hpp"
#include <QLabel>
#include <QSize>
#include <QTableWidget>
#include <QToolBar>
#include <QWidget>
#include <string>
#include <vector>

namespace ores::qt {

/**
 * @brief Read-only NxN cross-rates matrix for the current party's CRM
 * (Cross-rates matrix) configuration.
 *
 * Sourced entirely from a single `marketdata.v1.crm.rates` request --
 * manual reload only, by design: the CRM story's own architecture
 * decision is to never broadcast the full derived rate set, so this
 * panel deliberately has no auto-refresh timer and no NATS subscription
 * (contrast with ServiceDashboardMdiWindow's optional auto-refresh).
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
    explicit CrmCrossRatesMatrixMdiWindow(ClientManager* clientManager, QWidget* parent = nullptr);
    ~CrmCrossRatesMatrixMdiWindow() override = default;

    QSize sizeHint() const override {
        return QSize{760, 520};
    }

public slots:
    void reload();

signals:
    void statusChanged(const QString& message);
    void errorOccurred(const QString& error_message);

private:
    void setupUi();
    void setupToolbar();

    ClientManager* clientManager_;

    QToolBar* toolbar_;
    QAction* reloadAction_;
    QTableWidget* table_;
    QLabel* footerLabel_;
};

}

#endif
