/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CurrencyHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include "ui_CurrencyHistoryDialog.h"
#include <QCloseEvent>
#include <QHeaderView>
#include <QLabel>

namespace ores::qt {

using namespace ores::logging;

CurrencyHistoryDialog::CurrencyHistoryDialog(QString iso_code,
                                             ClientManager* clientManager,
                                             QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::CurrencyHistoryDialog)
    , clientManager_(clientManager)
    , imageCache_(nullptr)
    , isoCode_(std::move(iso_code)) {

    BOOST_LOG_SEV(lg(), info) << "Creating currency history widget for: " << isoCode_.toStdString();

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});

    ui_->detailsTableWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->detailsTableWidget->setColumnWidth(0, 200);

    resize(UiPersistence::restoreSize(QLatin1String("CurrencyHistoryDialog"), sizeHint()));
}

CurrencyHistoryDialog::~CurrencyHistoryDialog() = default;

void CurrencyHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading currency history for: " << isoCode_.toStdString();

    refdata::messaging::get_currency_history_request request;
    request.iso_code = isoCode_.toStdString();

    runHistoryRequest(clientManager_, std::move(request), [this](auto response) {
        if (!response.success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            historyLoadFailed(QString::fromStdString(response.message));
            return;
        }
        history_ = std::move(response.history);
        historyLoaded();
    });
}

int CurrencyHistoryDialog::historySize() const {
    return static_cast<int>(history_.versions.size());
}

HistoryDialogBase::VersionRow CurrencyHistoryDialog::versionRow(int index) const {
    const auto& version = history_.versions[index];
    return {.version = version.version_number,
            .cells = {relative_time_helper::format(version.recorded_at),
                      QString::fromStdString(version.modified_by),
                      QString::fromStdString(version.data.change_reason_code),
                      QString::fromStdString(version.data.change_commentary)}};
}

QString CurrencyHistoryDialog::historyTitle() const {
    const auto& latest = history_.versions.front();
    return QString("Currency History: %1 - %2")
        .arg(isoCode_)
        .arg(QString::fromStdString(latest.data.name));
}

HistoryDialogBase::DiffResult CurrencyHistoryDialog::calculateDiffAt(int current_index,
                                                                     int /*previous_index*/) const {
    // The server computes the diff; this renders its rows verbatim.
    DiffResult diffs;
    const auto& changes = history_.versions[current_index].changes;
    diffs.reserve(static_cast<qsizetype>(changes.entries.size()));
    for (const auto& entry : changes.entries) {
        diffs.append(
            {QString::fromStdString(entry.field_name),
             {QString::fromStdString(entry.old_value), QString::fromStdString(entry.new_value)}});
    }
    return diffs;
}

QWidget* CurrencyHistoryDialog::changeCellWidget(const QString& field, const QString& value) {
    // Show flag icons instead of image UUIDs.
    if (field != "Flag" || !imageCache_)
        return nullptr;

    auto* label = new QLabel();
    label->setAlignment(Qt::AlignCenter);
    label->setFixedSize(32, 32);

    if (value == "(none)") {
        QIcon noFlagIcon = imageCache_->getNoFlagIcon();
        if (!noFlagIcon.isNull())
            label->setPixmap(noFlagIcon.pixmap(24, 24));
        else
            label->setText("-");
    } else {
        QIcon icon = imageCache_->getIcon(value.toStdString());
        if (!icon.isNull()) {
            label->setPixmap(icon.pixmap(24, 24));
        } else {
            label->setText("?");
            label->setToolTip(value);
        }
    }
    return label;
}

void CurrencyHistoryDialog::displayFullDetails(int index) {
    const auto& version = history_.versions[index];

    auto* table = ui_->detailsTableWidget;
    table->setRowCount(0);
    table->setRowCount(static_cast<int>(version.fields.size()) + 3);

    int row = 0;
    auto addRow = [&](const QString& field, const QString& value) {
        table->setItem(row, 0, new QTableWidgetItem(field));
        if (QWidget* widget = changeCellWidget(field, value))
            table->setCellWidget(row, 1, widget);
        else
            table->setItem(row, 1, new QTableWidgetItem(value));
        ++row;
    };

    for (const auto& field : version.fields) {
        addRow(QString::fromStdString(field.name), QString::fromStdString(field.value));
    }

    addRow(tr("Version"), QString::number(version.version_number));
    addRow(tr("Modified By"), QString::fromStdString(version.modified_by));
    addRow(tr("Recorded At"), relative_time_helper::format(version.recorded_at));
}

void CurrencyHistoryDialog::openVersionAt(int index) {
    const auto& version = history_.versions[index];
    BOOST_LOG_SEV(lg(), info) << "Opening currency version " << version.version_number
                              << " in read-only mode";
    emit openVersionRequested(version.data, version.version_number);
}

void CurrencyHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the
    // selected version, stamped with the latest version number.
    const auto& selected = history_.versions[index];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version " << selected.version_number;

    refdata::domain::currency currency = selected.data;
    currency.version = history_.versions.front().version_number;
    emit revertVersionRequested(currency);
}

void CurrencyHistoryDialog::closeEvent(QCloseEvent* event) {
    UiPersistence::saveSize(QLatin1String("CurrencyHistoryDialog"), this);
    HistoryDialogBase::closeEvent(event);
}

void CurrencyHistoryDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
}

}
