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
#ifndef ORES_QT_PORTFOLIO_EXPLORER_TRADE_MODEL_HPP
#define ORES_QT_PORTFOLIO_EXPLORER_TRADE_MODEL_HPP

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <QFutureWatcher>
#include <QAbstractTableModel>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.trading/domain/trade.hpp"

namespace ores::qt {

/**
 * @brief Short counterparty display info.
 */
struct CounterpartyInfo {
    std::string short_code;
    std::string full_name;
};

/**
 * @brief Table model for filtered trades in the portfolio/book tree window.
 *
 * Mirrors ClientTradeModel but adds counterparty display columns and
 * optional book_id / portfolio_id filter fields.
 */
class PortfolioExplorerTradeModel final : public QAbstractTableModel {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.portfolio_explorer_trade_model";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum Column {
        ExternalId,
        TradeType,
        CounterpartyShortCode,
        CounterpartyName,
        LifecycleEvent,
        TradeDate,
        EffectiveDate,
        TerminationDate,
        Version,
        ModifiedBy,
        RecordedAt,
        ColumnCount
    };

    explicit PortfolioExplorerTradeModel(ClientManager* clientManager,
                                     QObject* parent = nullptr);
    ~PortfolioExplorerTradeModel() override = default;

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section, Qt::Orientation orientation,
        int role = Qt::DisplayRole) const override;

    /**
     * @brief Set book/portfolio filter and clear existing data.
     */
    void set_filter(std::optional<boost::uuids::uuid> book_id,
                    std::optional<boost::uuids::uuid> portfolio_id);

    /**
     * @brief Set the counterparty lookup map (UUID string -> CounterpartyInfo).
     */
    void set_counterparty_map(
        std::unordered_map<std::string, CounterpartyInfo> cpty_map);

    /**
     * @brief Refresh trade data from server using current filter.
     */
    void refresh();

    /**
     * @brief Load a specific page.
     */
    void load_page(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Get total available count from last fetch.
     */
    std::uint32_t total_available_count() const { return total_available_count_; }

    /**
     * @brief Get the trade at the given row.
     */
    const trading::domain::trade* get_trade(int row) const;

signals:
    void dataLoaded();
    void loadError(const QString& error_message, const QString& details = {});

private slots:
    void onTradesLoaded();

private:
    struct FetchResult {
        bool success;
        std::vector<trading::domain::trade> trades;
        std::uint32_t total_available_count;
        QString error_message;
        QString error_details;
    };

    void fetch_trades(std::uint32_t offset, std::uint32_t limit);

    ClientManager* clientManager_;
    std::vector<trading::domain::trade> trades_;
    QFutureWatcher<FetchResult>* watcher_;
    std::uint32_t total_available_count_{0};
    bool is_fetching_{false};

    std::optional<boost::uuids::uuid> filter_book_id_;
    std::optional<boost::uuids::uuid> filter_portfolio_id_;
    std::unordered_map<std::string, CounterpartyInfo> cpty_map_;
};

}

#endif
