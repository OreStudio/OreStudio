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
#include "ores.qt/ZeroConventionHistoryDialog.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_ZeroConventionHistoryDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata.api/messaging/zero_convention_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ZeroConventionHistoryDialog::ZeroConventionHistoryDialog(
    const QString& code,
    ClientManager* clientManager,
    QWidget* parent)
    : QWidget(parent),
      ui_(new Ui::ZeroConventionHistoryDialog),
      code_(code),
      clientManager_(clientManager),
      toolbar_(nullptr),
      openVersionAction_(nullptr),
      revertAction_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupToolbar();
    setupConnections();
}

ZeroConventionHistoryDialog::~ZeroConventionHistoryDialog() {
    delete ui_;
}

void ZeroConventionHistoryDialog::setupUi() {
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    ui_->titleLabel->setText(QString("History for: %1").arg(code_));

    // Setup version list table
    ui_->versionListWidget->setColumnCount(5);
    ui_->versionListWidget->setHorizontalHeaderLabels(
        {"Version", "Recorded At", "Modified By", "Performed By", "Commentary"});
    ui_->versionListWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->versionListWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui_->versionListWidget->setSelectionMode(QAbstractItemView::SingleSelection);

    // Setup changes table
    ui_->changesTableWidget->setColumnCount(3);
    ui_->changesTableWidget->setHorizontalHeaderLabels(
        {"Field", "Old Value", "New Value"});
    ui_->changesTableWidget->horizontalHeader()->setStretchLastSection(true);
}

void ZeroConventionHistoryDialog::setupToolbar() {
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

    // Insert toolbar at the top of the layout
    auto* layout = qobject_cast<QVBoxLayout*>(this->layout());
    if (layout) {
        layout->insertWidget(0, toolbar_);
    }
}

void ZeroConventionHistoryDialog::setupConnections() {
    connect(ui_->versionListWidget, &QTableWidget::itemSelectionChanged,
            this, &ZeroConventionHistoryDialog::onVersionSelected);
    connect(openVersionAction_, &QAction::triggered,
            this, &ZeroConventionHistoryDialog::onOpenVersionClicked);
    connect(revertAction_, &QAction::triggered,
            this, &ZeroConventionHistoryDialog::onRevertClicked);
    connect(ui_->closeButton, &QPushButton::clicked,
            this, [this]() { if (window()) window()->close(); });
}

void ZeroConventionHistoryDialog::loadHistory() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred("Not connected to server");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading history for zero convention: " << code_.toStdString();
    emit statusChanged(tr("Loading history..."));

    QPointer<ZeroConventionHistoryDialog> self = this;

    using HistoryResult = std::expected<refdata::messaging::get_zero_convention_history_response, std::string>;

    QFuture<HistoryResult> future =
        QtConcurrent::run([self, code = code_.toStdString()]() -> HistoryResult {
        if (!self || !self->clientManager_)
            return std::unexpected("Dialog closed");
        refdata::messaging::get_zero_convention_history_request request;
        request.id = code;
        auto result = self->clientManager_->process_authenticated_request(std::move(request));
        if (!result) return std::unexpected(result.error());
        return std::move(*result);
    });

    auto* watcher = new QFutureWatcher<HistoryResult>(self);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!result) {
            BOOST_LOG_SEV(lg(), error) << "History load failed: " << result.error();
            emit self->errorOccurred(QString::fromStdString(result.error()));
            return;
        }
        if (!result->success) {
            emit self->errorOccurred(QString::fromStdString(result->message));
            return;
        }
        self->versions_ = std::move(result->zero_conventions);
        self->updateVersionList();
        emit self->statusChanged(
            QString("Loaded %1 versions").arg(self->versions_.size()));
    });
    watcher->setFuture(future);
}

void ZeroConventionHistoryDialog::updateVersionList() {
    ui_->versionListWidget->setRowCount(0);

    for (const auto& version : versions_) {
        int row = ui_->versionListWidget->rowCount();
        ui_->versionListWidget->insertRow(row);

        auto* versionItem = new QTableWidgetItem(QString::number(version.version));
        versionItem->setTextAlignment(Qt::AlignCenter);
        ui_->versionListWidget->setItem(row, 0, versionItem);

        auto* recordedAtItem = new QTableWidgetItem(
            relative_time_helper::format(version.recorded_at));
        ui_->versionListWidget->setItem(row, 1, recordedAtItem);

        auto* modifiedByItem = new QTableWidgetItem(
            QString::fromStdString(version.modified_by));
        ui_->versionListWidget->setItem(row, 2, modifiedByItem);

        auto* performedByItem = new QTableWidgetItem(
            QString::fromStdString(version.performed_by));
        ui_->versionListWidget->setItem(row, 3, performedByItem);

        auto* commentaryItem = new QTableWidgetItem(
            QString::fromStdString(version.change_commentary));
        ui_->versionListWidget->setItem(row, 4, commentaryItem);
    }

    // Select the first (most recent) version
    if (!versions_.empty()) {
        ui_->versionListWidget->selectRow(0);
    }
}

void ZeroConventionHistoryDialog::onVersionSelected() {
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

void ZeroConventionHistoryDialog::updateChangesTable(int currentVersionIndex) {
    ui_->changesTableWidget->setRowCount(0);

    if (currentVersionIndex < 0 ||
        static_cast<size_t>(currentVersionIndex) >= versions_.size()) {
        return;
    }

    // Get the next older version to compare against
    int previousVersionIndex = currentVersionIndex + 1;
    if (static_cast<size_t>(previousVersionIndex) >= versions_.size()) {
        // This is the first version, no changes to show
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

    if (current.id != previous.id) {
        addChange("Id",
                  QString::fromStdString(previous.id),
                  QString::fromStdString(current.id));
    }

    if (current.day_count_fraction != previous.day_count_fraction) {
        addChange("Day Count Fraction",
                  QString::fromStdString(previous.day_count_fraction),
                  QString::fromStdString(current.day_count_fraction));
    }

    if (current.compounding != previous.compounding) {
        addChange("Compounding",
                  previous.compounding ? QString::fromStdString(*previous.compounding) : QString{},
                  current.compounding ? QString::fromStdString(*current.compounding) : QString{});
    }

    if (current.compounding_frequency != previous.compounding_frequency) {
        addChange("Compounding Frequency",
                  previous.compounding_frequency ? QString::fromStdString(*previous.compounding_frequency) : QString{},
                  current.compounding_frequency ? QString::fromStdString(*current.compounding_frequency) : QString{});
    }

    if (current.tenor_calendar != previous.tenor_calendar) {
        addChange("Tenor Calendar",
                  previous.tenor_calendar ? QString::fromStdString(*previous.tenor_calendar) : QString{},
                  current.tenor_calendar ? QString::fromStdString(*current.tenor_calendar) : QString{});
    }

    if (current.spot_calendar != previous.spot_calendar) {
        addChange("Spot Calendar",
                  previous.spot_calendar ? QString::fromStdString(*previous.spot_calendar) : QString{},
                  current.spot_calendar ? QString::fromStdString(*current.spot_calendar) : QString{});
    }

    if (current.roll_convention != previous.roll_convention) {
        addChange("Roll Convention",
                  previous.roll_convention ? QString::fromStdString(*previous.roll_convention) : QString{},
                  current.roll_convention ? QString::fromStdString(*current.roll_convention) : QString{});
    }


    if (ui_->changesTableWidget->rowCount() == 0) {
        ui_->changesTableWidget->insertRow(0);
        ui_->changesTableWidget->setItem(0, 0,
            new QTableWidgetItem("(No field changes)"));
        ui_->changesTableWidget->setItem(0, 1, new QTableWidgetItem("-"));
        ui_->changesTableWidget->setItem(0, 2, new QTableWidgetItem("-"));
    }
}

void ZeroConventionHistoryDialog::updateFullDetails(int versionIndex) {
    if (versionIndex < 0 ||
        static_cast<size_t>(versionIndex) >= versions_.size()) {
        return;
    }

    const auto& version = versions_[versionIndex];

    ui_->idValue->setText(QString::fromStdString(version.id));
    ui_->dayCountFractionValue->setText(QString::fromStdString(version.day_count_fraction));
    ui_->compoundingValue->setText(version.compounding
        ? QString::fromStdString(*version.compounding)
        : QString{});
    ui_->compoundingFrequencyValue->setText(version.compounding_frequency
        ? QString::fromStdString(*version.compounding_frequency)
        : QString{});
    ui_->tenorCalendarValue->setText(version.tenor_calendar
        ? QString::fromStdString(*version.tenor_calendar)
        : QString{});
    ui_->spotCalendarValue->setText(version.spot_calendar
        ? QString::fromStdString(*version.spot_calendar)
        : QString{});
    ui_->rollConventionValue->setText(version.roll_convention
        ? QString::fromStdString(*version.roll_convention)
        : QString{});
    ui_->versionNumberValue->setText(QString::number(version.version));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(version.recorded_at));
    ui_->changeCommentaryValue->setText(
        QString::fromStdString(version.change_commentary));
}

void ZeroConventionHistoryDialog::updateActionStates() {
    auto selected = ui_->versionListWidget->selectedItems();
    bool hasSelection = !selected.isEmpty();
    bool isNotLatest = hasSelection && selected.first()->row() > 0;

    openVersionAction_->setEnabled(hasSelection);
    revertAction_->setEnabled(isNotLatest);
}

void ZeroConventionHistoryDialog::onOpenVersionClicked() {
    auto selected = ui_->versionListWidget->selectedItems();
    if (selected.isEmpty()) return;

    int row = selected.first()->row();
    if (static_cast<size_t>(row) >= versions_.size()) return;

    emit openVersionRequested(versions_[row], versions_[row].version);
}

void ZeroConventionHistoryDialog::onRevertClicked() {
    auto selected = ui_->versionListWidget->selectedItems();
    if (selected.isEmpty()) return;

    int row = selected.first()->row();
    if (static_cast<size_t>(row) >= versions_.size()) return;

    emit revertVersionRequested(versions_[row]);
}

}
