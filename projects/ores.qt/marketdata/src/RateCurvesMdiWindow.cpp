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
#include "ores.qt/RateCurvesMdiWindow.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include <QFutureWatcher>
#include <QHeaderView>
#include <QLabel>
#include <QPointer>
#include <QSortFilterProxyModel>
#include <QStandardItemModel>
#include <QTableView>
#include <QToolBar>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>
#include <cctype>
#include <string_view>

namespace ores::qt {

using namespace ores::logging;

namespace {
namespace m = ores::marketdata::messaging;
namespace md = ores::marketdata::domain;

// GUI-side derivation of the currency code from the oresmd ir URI host
// (e.g. "eur" from oresmd://ir/eur?role=discount&type=curve), mirroring the
// oresmd layer -- service-layer code isn't linkable from Qt.
std::string leading_currency_code(const std::string& uri) {
    constexpr std::string_view prefix = "oresmd://ir/";
    if (uri.rfind(prefix, 0) != 0)
        return {};
    const auto host_start = prefix.size();
    const auto host_end = uri.find('?', host_start);
    auto ccy = uri.substr(
        host_start, host_end == std::string::npos ? std::string::npos : host_end - host_start);
    std::transform(ccy.begin(), ccy.end(), ccy.begin(), [](unsigned char c) {
        return std::toupper(c);
    });
    return ccy;
}

}

RateCurvesMdiWindow::RateCurvesMdiWindow(ClientManager* clientManager,
                                         ImageCache* imageCache,
                                         QWidget* parent)
    : QWidget(parent)
    , clientManager_(clientManager)
    , imageCache_(imageCache)
    , toolbar_(nullptr)
    , reloadAction_(nullptr)
    , tableView_(nullptr)
    , model_(nullptr)
    , proxyModel_(nullptr)
    , emptyLabel_(nullptr) {
    setupUi();
    reload();
}

void RateCurvesMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));
    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    reloadAction_->setToolTip(tr("Reload the list of rate curves"));
    connect(reloadAction_, &QAction::triggered, this, &RateCurvesMdiWindow::reload);
    layout->addWidget(toolbar_);

    model_ = new QStandardItemModel(0, 1, this);
    model_->setHorizontalHeaderLabels({tr("oresmd URI")});

    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->setIconSize(single_flag_icon_size());
    connect(tableView_, &QTableView::doubleClicked, this, &RateCurvesMdiWindow::onRowActivated);
    layout->addWidget(tableView_);

    emptyLabel_ = new QLabel(
        tr("No interest-rate series found yet. A series appears here once something has "
           "published at least one observation for it (any producer -- synthetic, a vendor "
           "feed, etc.)."),
        this);
    emptyLabel_->setAlignment(Qt::AlignCenter);
    emptyLabel_->setStyleSheet("color: gray; font-style: italic; padding: 24px;");
    emptyLabel_->setVisible(false);
    layout->addWidget(emptyLabel_);
}

void RateCurvesMdiWindow::reload() {
    emit statusChanged(tr("Loading rate curves..."));

    m::get_market_series_request req;
    req.offset = 0;
    req.limit = 500; // curated read-only view, not a paginated CRUD list

    QPointer<RateCurvesMdiWindow> self = this;
    auto* cm = clientManager_;

    struct Result {
        bool success = false;
        QString message;
        std::vector<md::market_series> series;
    };

    auto task = [cm, req]() -> Result {
        auto resp = cm->process_authenticated_request(req);
        if (!resp)
            return {false, QString::fromStdString(resp.error()), {}};
        if (!resp->success)
            return {false, QString::fromStdString(resp->message), {}};
        return {true, {}, std::move(resp->market_series)};
    };

    auto* watcher = new QFutureWatcher<Result>(self);
    connect(watcher, &QFutureWatcher<Result>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        if (!result.success) {
            emit self->errorOccurred(tr("Failed to load rate curves: %1").arg(result.message));
            return;
        }

        self->rows_.clear();
        for (const auto& s : result.series) {
            // IR curves only: the URI host is the asset class's currency and
            // type=curve marks a whole curve, not a fixing/quote/vol point.
            if (s.oresmd_uri.rfind("oresmd://ir/", 0) != 0 ||
                s.oresmd_uri.find("type=curve") == std::string::npos)
                continue;
            self->rows_.push_back({s.oresmd_uri});
        }

        const bool empty = self->rows_.empty();
        self->tableView_->setVisible(!empty);
        self->emptyLabel_->setVisible(empty);

        self->model_->removeRows(0, self->model_->rowCount());
        self->model_->setRowCount(static_cast<int>(self->rows_.size()));
        for (std::size_t i = 0; i < self->rows_.size(); ++i) {
            const auto& r = self->rows_[i];
            const int row = static_cast<int>(i);

            auto* uriItem = new QStandardItem(QString::fromStdString(r.oresmd_uri));
            if (self->imageCache_) {
                const auto ccy = leading_currency_code(r.oresmd_uri);
                if (!ccy.empty())
                    uriItem->setIcon(currency_flag_icon(*self->imageCache_, ccy));
            }
            self->model_->setItem(row, 0, uriItem);
        }
        self->tableView_->resizeColumnsToContents();

        emit self->statusChanged(tr("Loaded %1 rate curve(s).").arg(self->rows_.size()));
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void RateCurvesMdiWindow::onRowActivated(const QModelIndex& index) {
    if (!index.isValid())
        return;
    const auto sourceIndex = proxyModel_->mapToSource(index);
    const auto row = sourceIndex.row();
    if (row < 0 || static_cast<std::size_t>(row) >= rows_.size())
        return;
    const auto& r = rows_[row];
    emit viewSnapshotRequested(QString::fromStdString(r.oresmd_uri));
}

}
