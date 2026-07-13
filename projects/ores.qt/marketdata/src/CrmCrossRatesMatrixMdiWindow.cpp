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
#include "ores.qt/CrmCrossRatesMatrixMdiWindow.hpp"
#include "ores.marketdata.api/messaging/crm_protocol.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <QSet>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <map>

namespace ores::qt {

using namespace ores::logging;
namespace marketdata_msg = ores::marketdata::messaging;

namespace {

QTableWidgetItem* make_item(const QString& text) {
    auto* item = new QTableWidgetItem(text);
    item->setTextAlignment(Qt::AlignCenter);
    item->setFlags(item->flags() & ~Qt::ItemIsEditable);
    return item;
}

struct RatesResult {
    bool success{false};
    QString error;
    std::vector<marketdata_msg::crm_rate_item> rates;
};

} // namespace

CrmCrossRatesMatrixMdiWindow::CrmCrossRatesMatrixMdiWindow(ClientManager* clientManager,
                                                            QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , toolbar_(nullptr)
    , reloadAction_(nullptr)
    , table_(nullptr)
    , footerLabel_(nullptr) {

    setupUi();
    reload();
}

void CrmCrossRatesMatrixMdiWindow::setupUi() {
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(4, 4, 4, 4);
    mainLayout->setSpacing(4);

    setupToolbar();
    mainLayout->addWidget(toolbar_);

    table_ = new QTableWidget(this);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionMode(QAbstractItemView::NoSelection);
    table_->setAlternatingRowColors(true);
    table_->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
    table_->verticalHeader()->setSectionResizeMode(QHeaderView::Stretch);
    mainLayout->addWidget(table_);

    footerLabel_ = new QLabel(this);
    mainLayout->addWidget(footerLabel_);
}

void CrmCrossRatesMatrixMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    // Manual reload only -- no auto-refresh action, no timer. The CRM
    // architecture deliberately never broadcasts the derived rate set, so
    // this panel only ever pulls on an explicit click.
    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    reloadAction_->setToolTip(tr("Reload the cross-rates matrix"));
    connect(reloadAction_, &QAction::triggered, this, &CrmCrossRatesMatrixMdiWindow::reload);
}

void CrmCrossRatesMatrixMdiWindow::reload() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit statusChanged(tr("Not connected"));
        return;
    }

    reloadAction_->setEnabled(false);
    emit statusChanged(tr("Loading cross-rates matrix..."));

    const auto party_id = boost::uuids::to_string(clientManager_->currentPartyId());
    QPointer<CrmCrossRatesMatrixMdiWindow> self = this;

    QFuture<RatesResult> future = QtConcurrent::run([self, party_id]() -> RatesResult {
        if (!self || !self->clientManager_)
            return {};

        marketdata_msg::get_crm_rates_request req;
        req.party_id = party_id;
        auto resp = self->clientManager_->process_authenticated_request(req);

        RatesResult r;
        if (!resp) {
            r.error = QStringLiteral("Failed to communicate with server");
            return r;
        }
        if (!resp->success) {
            r.error = QString::fromStdString(resp->message);
            return r;
        }
        r.success = true;
        r.rates = resp->rates;
        return r;
    });

    auto* watcher = new QFutureWatcher<RatesResult>(this);
    connect(watcher, &QFutureWatcher<RatesResult>::finished, this, [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        self->reloadAction_->setEnabled(true);

        if (!result.success) {
            const QString msg =
                result.error.isEmpty() ? tr("Failed to load cross-rates matrix") : result.error;
            BOOST_LOG_SEV(lg(), error) << msg.toStdString();
            emit self->errorOccurred(msg);
            self->footerLabel_->setText(tr("DISCONNECTED"));
            return;
        }

        // Currencies are whatever the response actually covers, in
        // alphabetical order -- never a hardcoded/curated list.
        QSet<QString> currencySet;
        for (const auto& rate : result.rates) {
            currencySet.insert(QString::fromStdString(rate.base_currency_code));
            currencySet.insert(QString::fromStdString(rate.quote_currency_code));
        }
        QStringList currencies(currencySet.begin(), currencySet.end());
        std::sort(currencies.begin(), currencies.end());

        std::map<std::pair<QString, QString>, marketdata_msg::crm_rate_item> byPair;
        for (const auto& rate : result.rates) {
            byPair[{QString::fromStdString(rate.base_currency_code),
                   QString::fromStdString(rate.quote_currency_code)}] = rate;
        }

        self->table_->clear();
        self->table_->setRowCount(currencies.size());
        self->table_->setColumnCount(currencies.size());
        self->table_->setHorizontalHeaderLabels(currencies);
        self->table_->setVerticalHeaderLabels(currencies);

        for (int row = 0; row < currencies.size(); ++row) {
            for (int col = 0; col < currencies.size(); ++col) {
                if (row == col) {
                    self->table_->setItem(row, col, make_item(QStringLiteral("-")));
                    continue;
                }

                const auto key = std::make_pair(currencies[row], currencies[col]);
                const auto it = byPair.find(key);
                if (it == byPair.end()) {
                    self->table_->setItem(row, col, make_item(QStringLiteral("-")));
                    continue;
                }

                const auto& item = it->second;
                auto* cell = make_item(QString::number(item.rate, 'f', 5));
                if (item.status == "stale") {
                    cell->setForeground(color_constants::level_warn);
                    cell->setToolTip(tr("Stale as of %1").arg(QString::fromStdString(item.as_of)));
                } else if (item.status == "unavailable") {
                    cell->setForeground(color_constants::level_trace);
                    cell->setToolTip(tr("Unavailable"));
                } else {
                    cell->setToolTip(tr("Fresh as of %1").arg(QString::fromStdString(item.as_of)));
                }
                self->table_->setItem(row, col, cell);
            }
        }

        self->footerLabel_->setText(
            tr("CONNECTED | %1 Currencies").arg(currencies.size()));

        BOOST_LOG_SEV(lg(), debug)
            << "CRM cross-rates matrix: " << result.rates.size() << " rate(s), "
            << currencies.size() << " currencies";
        emit self->statusChanged(
            tr("Cross-rates matrix updated: %1 rate(s)").arg(result.rates.size()));
    });
    watcher->setFuture(future);
}

}
