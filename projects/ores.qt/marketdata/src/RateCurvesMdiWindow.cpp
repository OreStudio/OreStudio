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
#include "ores.marketdata.api/domain/asset_class.hpp"
#include "ores.marketdata.api/domain/series_subclass.hpp"
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

namespace ores::qt {

using namespace ores::logging;

namespace {
namespace m = ores::marketdata::messaging;
namespace md = ores::marketdata::domain;

// For a RATES/YIELD series, qualifier is documented as "currency/index" -- the ORE-key
// segment structure, a producer contract (see ir_curve_tick::qualifier), not a guess.
std::string leading_currency_code(const std::string& qualifier) {
    const auto sep = qualifier.find('/');
    return sep == std::string::npos ? std::string{} : qualifier.substr(0, sep);
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

    model_ = new QStandardItemModel(0, 4, this);
    model_->setHorizontalHeaderLabels(
        {tr("Series Type"), tr("Metric"), tr("Qualifier"), tr("Subclass")});

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
            if (s.asset_class != md::asset_class::rates || s.is_scalar)
                continue;
            self->rows_.push_back({s.series_type,
                                   s.metric,
                                   s.qualifier,
                                   s.series_subclass == md::series_subclass::yield ? "yield" :
                                   s.series_subclass == md::series_subclass::basis ? "basis" :
                                   s.series_subclass == md::series_subclass::fra   ? "fra" :
                                   s.series_subclass == md::series_subclass::xccy  ? "xccy" :
                                                                                     "other"});
        }

        const bool empty = self->rows_.empty();
        self->tableView_->setVisible(!empty);
        self->emptyLabel_->setVisible(empty);

        self->model_->removeRows(0, self->model_->rowCount());
        self->model_->setRowCount(static_cast<int>(self->rows_.size()));
        for (std::size_t i = 0; i < self->rows_.size(); ++i) {
            const auto& r = self->rows_[i];
            const int row = static_cast<int>(i);

            self->model_->setItem(row, 0, new QStandardItem(QString::fromStdString(r.series_type)));
            self->model_->setItem(row, 1, new QStandardItem(QString::fromStdString(r.metric)));

            auto* qualifierItem = new QStandardItem(QString::fromStdString(r.qualifier));
            if (self->imageCache_) {
                const auto ccy = leading_currency_code(r.qualifier);
                if (!ccy.empty())
                    qualifierItem->setIcon(currency_flag_icon(*self->imageCache_, ccy));
            }
            self->model_->setItem(row, 2, qualifierItem);

            self->model_->setItem(
                row, 3, new QStandardItem(QString::fromStdString(r.series_subclass)));
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
    emit viewSnapshotRequested(QString::fromStdString(r.series_type),
                               QString::fromStdString(r.metric),
                               QString::fromStdString(r.qualifier));
}

}
