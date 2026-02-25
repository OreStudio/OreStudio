/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_CLIENT_CURRENCY_MODEL_HPP
#define ORES_QT_CLIENT_CURRENCY_MODEL_HPP

#include <vector>
#include <unordered_set>
#include <QSize>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/RecencyPulseManager.hpp"
#include "ores.qt/RecencyTracker.hpp"
#include "ores.qt/ColumnMetadata.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/currency.hpp"

namespace ores::qt {

class ImageCache;

/**
 * @brief Model for displaying currencies fetched from the server via client.
 *
 * This model extends QAbstractTableModel and fetches currency data
 * asynchronously using the ores.comms client instead of direct database access.
 */
class ClientCurrencyModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_currency_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Enumeration of table columns for type-safe column access.
     *
     * Using an enum instead of magic numbers makes the code self-documenting
     * and easier to refactor when columns are added, removed, or reordered.
     */
    enum Column {
        IsoCode,
        CurrencyName,
        NumericCode,
        Symbol,
        FractionSymbol,
        FractionsPerUnit,
        RoundingType,
        RoundingPrecision,
        Format,
        MonetaryNature,
        MarketTier,
        Version,
        ModifiedBy,
        RecordedAt,
        ColumnCount  // Must be last - represents total number of columns
    };

    /**
     * @brief Column metadata: header text, style, visibility, and width.
     *
     * Order must match the Column enum.
     */
    static constexpr std::size_t kColumnCount = std::size_t(ColumnCount);
    static constexpr std::array<ColumnMetadata, kColumnCount> kColumns = {{
        {
            .column = IsoCode,
            .header = std::string_view("Code"),
            .style = column_style::mono_bold_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = CurrencyName,
            .header = std::string_view("Currency Name"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto},
        {
            .column = NumericCode,
            .header = std::string_view("Numeric Code"),
            .style = column_style::mono_center,
            .hidden_by_default = false,
            .default_width = 70
        },
        {
            .column = Symbol,
            .header = std::string_view("Symbol"),
            .style = column_style::mono_center,
            .hidden_by_default = false,
            .default_width = 60
        },
        {
            .column = FractionSymbol,
            .header = std::string_view("Fraction"),
            .style = column_style::mono_center,
            .hidden_by_default = true,
            .default_width = 60
        },
        {
            .column = FractionsPerUnit,
            .header = std::string_view("Per Unit"),
            .style = column_style::mono_right,
            .hidden_by_default = true,
            .default_width = 70
        },
        {
            .column = RoundingType,
            .header = std::string_view("Rounding Type"),
            .style = column_style::text_left,
            .hidden_by_default = true,
            .default_width = kColumnWidthAuto
        },
        {
            .column = RoundingPrecision,
            .header = std::string_view("Precision"),
            .style = column_style::mono_right,
            .hidden_by_default = true,
            .default_width = 70
        },
        {
            .column = Format,
            .header = std::string_view("Format"),
            .style = column_style::text_left,
            .hidden_by_default = true,
            .default_width = kColumnWidthAuto
        },
        {
            .column = MonetaryNature,
            .header = std::string_view("Monetary Nature"),
            .style = column_style::text_left,
            .hidden_by_default = true,
            .default_width = kColumnWidthAuto
        },
        {
            .column = MarketTier,
            .header = std::string_view("Market Tier"),
            .style = column_style::text_left,
            .hidden_by_default = true,
            .default_width = kColumnWidthAuto
        },
        {
            .column = Version,
            .header = std::string_view("Version"),
            .style = column_style::mono_center,
            .hidden_by_default = false,
            .default_width = 70
        },
        {
            .column = ModifiedBy,
            .header = std::string_view("Modified By"),
            .style = column_style::text_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
        {
            .column = RecordedAt,
            .header = std::string_view("Recorded At"),
            .style = column_style::mono_left,
            .hidden_by_default = false,
            .default_width = kColumnWidthAuto
        },
    }};

    /**
     * @brief Default window size for the currency list window.
     */
    inline static const QSize kDefaultWindowSize = {1000, 600};

    /**
     * @brief Settings group name for persisting window and column state.
     */
    static constexpr std::string_view kSettingsGroup = "CurrencyListWindow";

    /**
     * @brief Returns a static vector of column styles (built once per process).
     */
    static std::vector<column_style> const& columnStyles() {
        static std::vector<column_style> const kStylesVector = []() {
            std::vector<column_style> result;
            result.reserve(kColumnCount);
            for (std::size_t i = 0; i < kColumnCount; ++i)
                result.push_back(kColumns[i].style);
            return result;
        }();
        return kStylesVector;
    }

    /**
     * @brief Returns a static QVector of hidden column indices (built once per process).
     */
    static QVector<int> defaultHiddenColumns() {
        static QVector<int> const result =
            ::ores::qt::defaultHiddenColumns<kColumnCount>(kColumns);
        return result;
    }

    explicit ClientCurrencyModel(ClientManager* clientManager,
                                   ImageCache* imageCache,
                                   QObject* parent = nullptr);
    ~ClientCurrencyModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Refresh currency data from server asynchronously.
     *
     * This method initiates an async request to fetch currencies.
     * The model will emit dataChanged() when the fetch completes.
     */
    void refresh(bool replace = true);

    /**
     * @brief Load a specific page of currency data.
     *
     * Used for pagination navigation. Replaces current data with the
     * requested page.
     *
     * @param offset Number of records to skip
     * @param limit Number of records to fetch
     */
    void load_page(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Get currency at the specified row.
     *
     * @param row The row index.
     * @return The currency object, or nullptr if row is invalid.
     */
    const refdata::domain::currency* getCurrency(int row) const;

    /**
     * @brief Get all currencies.
     *
     * @return A vector containing all current currencies.
     */
    std::vector<refdata::domain::currency> getCurrencies() const;

    /**
     * @brief Get the page size used for pagination.
     *
     * @return The number of records fetched per page.
     */
    std::uint32_t page_size() const { return page_size_; }

    /**
     * @brief Set the page size for pagination.
     *
     * @param size The number of records to fetch per page (1-1000).
     */
    void set_page_size(std::uint32_t size);

    /**
     * @brief Get the total number of records available on the server.
     *
     * @return Total available record count.
     */
    std::uint32_t total_available_count() const { return total_available_count_; }

    /**
     * @brief Add synthetic (generated) currencies to the model.
     *
     * These currencies are displayed with a distinct color to indicate
     * they haven't been saved to the server yet.
     *
     * @param currencies The generated currencies to add.
     */
    void add_synthetic_currencies(std::vector<refdata::domain::currency> currencies);

    /**
     * @brief Check if a currency is synthetic (generated but not saved).
     *
     * @param iso_code The ISO code to check.
     * @return true if the currency is synthetic.
     */
    bool is_synthetic(const std::string& iso_code) const;

    /**
     * @brief Mark a synthetic currency as saved (no longer synthetic).
     *
     * Called after a generated currency has been successfully saved to server.
     *
     * @param iso_code The ISO code of the saved currency.
     */
    void mark_as_saved(const std::string& iso_code);

    /**
     * @brief Clear all synthetic currency markers.
     *
     * Called when refreshing data from server.
     */
    void clear_synthetic_markers();

signals:
    /**
     * @brief Emitted when data has been successfully loaded.
     */
    void dataLoaded();

    /**
     * @brief Emitted when an error occurs during data loading.
     */
    void loadError(const QString& error_message, const QString& details = {});

private slots:
    void onCurrenciesLoaded();
    void onPulseStateChanged(bool isOn);
    void onPulsingComplete();

private:
    /**
     * @brief Calculate foreground color based on currency state.
     *
     * Returns a color based on:
     * - Blue for synthetic (generated but not saved) currencies
     * - Yellow for recently modified currencies (pulsing effect)
     * - Default color otherwise
     *
     * @param iso_code The currency's ISO code to check.
     * @return QVariant containing QColor for foreground, or invalid QVariant if no color.
     */
    QVariant foreground_color(const std::string& iso_code) const;

    struct FetchResult {
        bool success;
        std::vector<refdata::domain::currency> currencies;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    using FutureWatcherResult = FetchResult;

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    std::vector<refdata::domain::currency> currencies_;
    QFutureWatcher<FutureWatcherResult>* watcher_;
    std::uint32_t page_size_{100};
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};

    // Recency highlighting
    using CurrencyKeyExtractor = std::string(*)(const refdata::domain::currency&);
    RecencyTracker<refdata::domain::currency, CurrencyKeyExtractor> recencyTracker_;
    RecencyPulseManager* pulseManager_;

    /**
     * @brief Internal method to fetch currencies with specific offset and limit.
     */
    void fetch_currencies(std::uint32_t offset, std::uint32_t limit);

    // Synthetic currency tracking (generated but not yet saved)
    std::unordered_set<std::string> synthetic_iso_codes_;
};

}

#endif
