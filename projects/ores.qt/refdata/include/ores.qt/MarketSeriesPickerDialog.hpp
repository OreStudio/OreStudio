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
#ifndef ORES_QT_MARKET_SERIES_PICKER_DIALOG_HPP
#define ORES_QT_MARKET_SERIES_PICKER_DIALOG_HPP

#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.qt/RefdataExport.hpp"
#include <QDialog>
#include <optional>
#include <vector>

class QLineEdit;
class QTableWidget;
class QPushButton;
class QComboBox;
class QLabel;

namespace ores::qt {

class ClientManager;

/**
 * @brief Modal picker over existing market_series catalog rows, with an inline "New Series..."
 * form for the common case where the desired series doesn't exist yet (e.g. an output curve
 * series before its first Publish -- curve_republish_service::compute() requires the output
 * series to already be catalogued). Used by CurveBuilderWorkbench for Source/Output Series Id
 * instead of a hand-typed UUID.
 */
class ORES_QT_REFDATA_EXPORT MarketSeriesPickerDialog : public QDialog {
    Q_OBJECT

public:
    /// Tunables for a specific call site -- e.g. Source vs. Output Series want different New
    /// Series defaults (raw grid vs. published curve conventions) so they don't look
    /// interchangeable, and Output additionally excludes whatever was already picked as Source so
    /// the two can never accidentally end up pointing at the same market_series row (which would
    /// otherwise let curve_republish_service reclassify the raw input series as its own output --
    /// see ir_curve_bootstrap_config_service::save_bootstrap_config()'s own guard against this).
    struct Options {
        /// Pre-fills the search box and the New Series qualifier field, e.g. an index qualifier
        /// such as "USD/SOFR" -- narrows the list to that index's series by construction, rather
        /// than showing every FX/rates/credit series in the tenant unfiltered.
        QString initialFilter;
        /// Empty means fall back to "RATES" -- kept a plain empty default (rather than "RATES"
        /// as a default member initializer) since a default member initializer here would need
        /// Options to be a complete type at the constructor declaration's `= {}` below, which it
        /// isn't yet inside the enclosing class body.
        QString defaultSeriesType;
        /// Empty means fall back to "YIELD" -- see defaultSeriesType's own comment.
        QString defaultMetric;
        /// Never shown in the existing-rows list, e.g. the Source series already picked -- empty
        /// string means no exclusion.
        QString excludeSeriesId;
    };

    MarketSeriesPickerDialog(ClientManager* clientManager,
                             QWidget* parent = nullptr,
                             const Options& options = {});

    /// Set once the user picks an existing row or successfully creates+selects a new one.
    [[nodiscard]] std::optional<marketdata::domain::market_series> selectedSeries() const {
        return selected_;
    }

private slots:
    void reload();
    void onRowActivated(int row);
    void onSelectClicked();
    void onCreateClicked();

private:
    QWidget* buildCreatePanel();
    void populateTable();
    static QString display_label(const marketdata::domain::market_series& s);

    ClientManager* clientManager_;
    Options options_;
    QLineEdit* filterEdit_ = nullptr;
    QTableWidget* table_ = nullptr;
    QPushButton* selectButton_ = nullptr;
    QLabel* statusLabel_ = nullptr;

    QLineEdit* newSeriesTypeEdit_ = nullptr;
    QLineEdit* newMetricEdit_ = nullptr;
    QLineEdit* newQualifierEdit_ = nullptr;
    QComboBox* newAssetClassCombo_ = nullptr;
    QComboBox* newSubclassCombo_ = nullptr;

    std::vector<marketdata::domain::market_series> rows_;
    std::optional<marketdata::domain::market_series> selected_;
};

}

#endif
