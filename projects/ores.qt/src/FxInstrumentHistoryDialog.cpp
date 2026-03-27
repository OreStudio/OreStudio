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
#include "ores.qt/FxInstrumentHistoryDialog.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_FxInstrumentHistoryDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

FxInstrumentHistoryDialog::FxInstrumentHistoryDialog(
    const QString& id,
    ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      ui_(new Ui::FxInstrumentHistoryDialog),
      id_(id),
      clientManager_(clientManager),
      toolbar_(nullptr),
      openVersionAction_(nullptr),
      revertAction_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupToolbar();
    setupConnections();
}

FxInstrumentHistoryDialog::~FxInstrumentHistoryDialog() {
    delete ui_;
}

void FxInstrumentHistoryDialog::setupUi() {
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(
            Icon::Dismiss, IconUtils::DefaultIconColor));
    ui_->titleLabel->setText(QString("History for: %1").arg(id_));

    ui_->versionListWidget->setColumnCount(4);
    ui_->versionListWidget->setHorizontalHeaderLabels(
        {"Version", "Recorded At", "Modified By", "Commentary"});
    ui_->versionListWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->versionListWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui_->versionListWidget->setSelectionMode(QAbstractItemView::SingleSelection);

    ui_->changesTableWidget->setColumnCount(3);
    ui_->changesTableWidget->setHorizontalHeaderLabels(
        {"Field", "Old Value", "New Value"});
    ui_->changesTableWidget->horizontalHeader()->setStretchLastSection(true);
}

void FxInstrumentHistoryDialog::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    openVersionAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Open, IconUtils::DefaultIconColor),
        tr("Open"));
    openVersionAction_->setToolTip(tr("Open this version (read-only)"));
    openVersionAction_->setEnabled(false);

    revertAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor),
        tr("Revert"));
    revertAction_->setToolTip(tr("Revert to this version"));
    revertAction_->setEnabled(false);

    auto* layout = qobject_cast<QVBoxLayout*>(this->layout());
    if (layout) layout->insertWidget(0, toolbar_);
}

void FxInstrumentHistoryDialog::setupConnections() {
    connect(ui_->versionListWidget, &QTableWidget::itemSelectionChanged,
            this, &FxInstrumentHistoryDialog::onVersionSelected);
    connect(openVersionAction_, &QAction::triggered,
            this, &FxInstrumentHistoryDialog::onOpenVersionClicked);
    connect(revertAction_, &QAction::triggered,
            this, &FxInstrumentHistoryDialog::onRevertClicked);
    connect(ui_->closeButton, &QPushButton::clicked,
            this, [this]() { if (window()) window()->close(); });
}

void FxInstrumentHistoryDialog::loadHistory() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    emit statusChanged(tr("Loading history..."));
    QPointer<FxInstrumentHistoryDialog> self = this;

    struct HistoryResult {
        bool success;
        std::string message;
        std::vector<trading::domain::fx_instrument> versions;
    };

    auto task = [self, id = id_.toStdString()]() -> HistoryResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed", {}};
        }

        trading::messaging::get_fx_instrument_history_request request;
        request.id = id;
        auto response_result =
            self->clientManager_->process_authenticated_request(
                std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server", {}};
        }

        return {response_result->success, response_result->message,
                std::move(response_result->history)};
    };

    auto* watcher = new QFutureWatcher<HistoryResult>(self);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            self->versions_ = std::move(result.versions);
            self->updateVersionList();
            emit self->statusChanged(
                QString("Loaded %1 versions").arg(self->versions_.size()));
        } else {
            BOOST_LOG_SEV(lg(), error) << "History load failed: "
                                       << result.message;
            emit self->errorOccurred(
                QString::fromStdString(result.message));
        }
    });

    QFuture<HistoryResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void FxInstrumentHistoryDialog::updateVersionList() {
    ui_->versionListWidget->setRowCount(0);

    for (const auto& version : versions_) {
        int row = ui_->versionListWidget->rowCount();
        ui_->versionListWidget->insertRow(row);

        auto* versionItem =
            new QTableWidgetItem(QString::number(version.version));
        versionItem->setTextAlignment(Qt::AlignCenter);
        ui_->versionListWidget->setItem(row, 0, versionItem);

        ui_->versionListWidget->setItem(row, 1, new QTableWidgetItem(
            relative_time_helper::format(version.recorded_at)));
        ui_->versionListWidget->setItem(row, 2, new QTableWidgetItem(
            QString::fromStdString(version.modified_by)));
        ui_->versionListWidget->setItem(row, 3, new QTableWidgetItem(
            QString::fromStdString(version.change_commentary)));
    }

    if (!versions_.empty()) {
        ui_->versionListWidget->selectRow(0);
    }
}

void FxInstrumentHistoryDialog::onVersionSelected() {
    auto selected = ui_->versionListWidget->selectedItems();
    if (selected.isEmpty()) {
        updateActionStates();
        return;
    }

    int row = selected.first()->row();
    updateChangesTable(row);
    updateFullDetails(row);
    updateActionStates();
}

void FxInstrumentHistoryDialog::updateChangesTable(int currentVersionIndex) {
    ui_->changesTableWidget->setRowCount(0);

    if (currentVersionIndex < 0 ||
        static_cast<std::size_t>(currentVersionIndex) >= versions_.size()) {
        return;
    }

    int previousVersionIndex = currentVersionIndex + 1;
    if (static_cast<std::size_t>(previousVersionIndex) >= versions_.size()) {
        ui_->changesTableWidget->insertRow(0);
        ui_->changesTableWidget->setItem(0, 0,
            new QTableWidgetItem("(Initial version)"));
        ui_->changesTableWidget->setItem(0, 1, new QTableWidgetItem("-"));
        ui_->changesTableWidget->setItem(0, 2, new QTableWidgetItem("-"));
        return;
    }

    const auto& current = versions_[currentVersionIndex];
    const auto& previous = versions_[previousVersionIndex];

    auto addChange = [this](const QString& field,
                            const QString& oldVal, const QString& newVal) {
        int row = ui_->changesTableWidget->rowCount();
        ui_->changesTableWidget->insertRow(row);
        ui_->changesTableWidget->setItem(row, 0, new QTableWidgetItem(field));
        ui_->changesTableWidget->setItem(row, 1, new QTableWidgetItem(oldVal));
        ui_->changesTableWidget->setItem(row, 2, new QTableWidgetItem(newVal));
    };

    if (current.trade_type_code != previous.trade_type_code)
        addChange("Trade Type",
            QString::fromStdString(previous.trade_type_code),
            QString::fromStdString(current.trade_type_code));

    if (current.bought_currency != previous.bought_currency)
        addChange("Bought Currency",
            QString::fromStdString(previous.bought_currency),
            QString::fromStdString(current.bought_currency));

    if (current.bought_amount != previous.bought_amount)
        addChange("Bought Amount",
            QString::number(previous.bought_amount, 'f', 2),
            QString::number(current.bought_amount, 'f', 2));

    if (current.sold_currency != previous.sold_currency)
        addChange("Sold Currency",
            QString::fromStdString(previous.sold_currency),
            QString::fromStdString(current.sold_currency));

    if (current.sold_amount != previous.sold_amount)
        addChange("Sold Amount",
            QString::number(previous.sold_amount, 'f', 2),
            QString::number(current.sold_amount, 'f', 2));

    if (current.value_date != previous.value_date)
        addChange("Value Date",
            QString::fromStdString(previous.value_date),
            QString::fromStdString(current.value_date));

    if (current.settlement != previous.settlement)
        addChange("Settlement",
            QString::fromStdString(previous.settlement),
            QString::fromStdString(current.settlement));

    if (current.option_type != previous.option_type)
        addChange("Option Type",
            QString::fromStdString(previous.option_type),
            QString::fromStdString(current.option_type));

    if (current.description != previous.description)
        addChange("Description",
            QString::fromStdString(previous.description),
            QString::fromStdString(current.description));

    if (ui_->changesTableWidget->rowCount() == 0) {
        ui_->changesTableWidget->insertRow(0);
        ui_->changesTableWidget->setItem(0, 0,
            new QTableWidgetItem("(No field changes)"));
        ui_->changesTableWidget->setItem(0, 1, new QTableWidgetItem("-"));
        ui_->changesTableWidget->setItem(0, 2, new QTableWidgetItem("-"));
    }
}

void FxInstrumentHistoryDialog::updateFullDetails(int versionIndex) {
    if (versionIndex < 0 ||
        static_cast<std::size_t>(versionIndex) >= versions_.size()) {
        return;
    }

    const auto& v = versions_[versionIndex];
    ui_->tradeTypeCodeValue->setText(
        QString::fromStdString(v.trade_type_code));
    ui_->boughtCurrencyValue->setText(
        QString::fromStdString(v.bought_currency));
    ui_->boughtAmountValue->setText(
        QString::number(v.bought_amount, 'f', 2));
    ui_->soldCurrencyValue->setText(
        QString::fromStdString(v.sold_currency));
    ui_->soldAmountValue->setText(
        QString::number(v.sold_amount, 'f', 2));
    ui_->valueDateValue->setText(
        QString::fromStdString(v.value_date));
    ui_->versionNumberValue->setText(QString::number(v.version));
    ui_->modifiedByValue->setText(
        QString::fromStdString(v.modified_by));
    ui_->recordedAtValue->setText(
        relative_time_helper::format(v.recorded_at));
    ui_->changeCommentaryValue->setText(
        QString::fromStdString(v.change_commentary));
}

void FxInstrumentHistoryDialog::updateActionStates() {
    auto selected = ui_->versionListWidget->selectedItems();
    bool hasSelection = !selected.isEmpty();
    bool isNotLatest = hasSelection && selected.first()->row() > 0;
    openVersionAction_->setEnabled(hasSelection);
    revertAction_->setEnabled(isNotLatest);
}

void FxInstrumentHistoryDialog::onOpenVersionClicked() {
    auto selected = ui_->versionListWidget->selectedItems();
    if (selected.isEmpty()) return;
    int row = selected.first()->row();
    if (static_cast<std::size_t>(row) >= versions_.size()) return;
    emit openVersionRequested(versions_[row], versions_[row].version);
}

void FxInstrumentHistoryDialog::onRevertClicked() {
    auto selected = ui_->versionListWidget->selectedItems();
    if (selected.isEmpty()) return;
    int row = selected.first()->row();
    if (static_cast<std::size_t>(row) >= versions_.size()) return;
    emit revertVersionRequested(versions_[row]);
}

}
