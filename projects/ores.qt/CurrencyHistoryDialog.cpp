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
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QDateTime>
#include <QIcon>
#include <QScrollBar>

namespace ores::qt {

using namespace ores::utility::log;

CurrencyHistoryDialog::CurrencyHistoryDialog(const QString& iso_code,
                                             std::shared_ptr<comms::client> client,
                                             QWidget* parent)
    : QWidget(parent),
      ui_(new Ui::CurrencyHistoryDialog),
      client_(std::move(client)),
      isoCode_(iso_code) {

    BOOST_LOG_SEV(lg(), info) << "Creating currency history widget for: "
                              << isoCode_.toStdString();

    ui_->setupUi(this);

    // Connect version list selection
    connect(ui_->versionListWidget, &QTableWidget::currentCellChanged,
            this, [this](int currentRow, int, int, int) {
        onVersionSelected(currentRow);
    });

    // Apply same styling as currencies table
    ui_->versionListWidget->setAlternatingRowColors(true);
    ui_->versionListWidget->setSelectionMode(QAbstractItemView::SingleSelection);
    ui_->versionListWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui_->versionListWidget->resizeRowsToContents();

    // Configure version table headers (same as currencies table)
    QHeaderView* versionVerticalHeader = ui_->versionListWidget->verticalHeader();
    QHeaderView* versionHorizontalHeader = ui_->versionListWidget->horizontalHeader();
    versionVerticalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);
    versionHorizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    // Set up changes table headers
    ui_->changesTableWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->changesTableWidget->setColumnWidth(0, 200);
    ui_->changesTableWidget->setColumnWidth(1, 200);
}

CurrencyHistoryDialog::~CurrencyHistoryDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying currency history widget";

    // Disconnect all signal connections to prevent callbacks during destruction
    if (ui_ && ui_->versionListWidget) {
        disconnect(ui_->versionListWidget, nullptr, this, nullptr);
    }

    delete ui_;
}

void CurrencyHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading currency history for: "
                              << isoCode_.toStdString();

    // Create request
    risk::messaging::get_currency_history_request request{isoCode_.toStdString()};
    auto payload = request.serialize();

    comms::protocol::frame request_frame(
        comms::protocol::message_type::get_currency_history_request,
        0,
        std::move(payload)
    );

    // Send request asynchronously
    using HistoryResult = std::expected<comms::protocol::frame, std::string>;
    QFuture<HistoryResult> future = QtConcurrent::run([this, frame = std::move(request_frame)]() mutable -> HistoryResult {
        auto response_result = client_->send_request_sync(std::move(frame));
        if (!response_result) {
            return std::unexpected("Failed to communicate with server");
        }
        return *response_result;
    });

    // Use watcher to handle results
    auto* watcher = new QFutureWatcher<HistoryResult>(this);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished,
            this, [this, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!result) {
            onHistoryLoadError(QString::fromStdString(result.error()));
            return;
        }

        // Check if server sent an error_response instead
        if (result->header().type != comms::protocol::message_type::get_currency_history_response) {
            onHistoryLoadError(QString("Server does not support currency history (received message type %1)")
                .arg(static_cast<int>(result->header().type)));
            return;
        }

        auto response = risk::messaging::get_currency_history_response::deserialize(
            result->payload()
        );

        if (!response) {
            onHistoryLoadError("Invalid server response");
            return;
        }

        if (!response->success) {
            onHistoryLoadError(QString::fromStdString(response->message));
            return;
        }

        history_ = std::move(response->history);
        onHistoryLoaded();
    });

    watcher->setFuture(future);
}

void CurrencyHistoryDialog::onHistoryLoaded() {
    BOOST_LOG_SEV(lg(), info) << "History loaded successfully: "
                              << history_.versions.size() << " versions";

    // Clear existing items
    ui_->versionListWidget->setRowCount(0);

    // Add each version to the table
    ui_->versionListWidget->setRowCount(history_.versions.size());
    for (int i = 0; i < static_cast<int>(history_.versions.size()); ++i) {
        const auto& version = history_.versions[i];

        auto* versionItem = new QTableWidgetItem(QString::number(version.version_number));
        auto* modifiedAtItem = new QTableWidgetItem(QString::fromStdString(version.modified_at));
        auto* modifiedByItem = new QTableWidgetItem(QString::fromStdString(version.modified_by));

        // Add icon to version column
        versionItem->setIcon(QIcon(":/icons/resources/icons/ic_fluent_history_20_regular.svg"));

        ui_->versionListWidget->setItem(i, 0, versionItem);
        ui_->versionListWidget->setItem(i, 1, modifiedAtItem);
        ui_->versionListWidget->setItem(i, 2, modifiedByItem);
    }

    // Select first version if available
    if (!history_.versions.empty()) {
        ui_->versionListWidget->selectRow(0);
    }

    // Update title with currency name
    if (!history_.versions.empty()) {
        const auto& latest = history_.versions[0];
        ui_->titleLabel->setText(QString("Currency History: %1 - %2")
            .arg(isoCode_)
            .arg(QString::fromStdString(latest.data.name)));
    }

    emit statusChanged(QString("Loaded %1 versions").arg(history_.versions.size()));
}

void CurrencyHistoryDialog::onHistoryLoadError(const QString& error_msg) {
    BOOST_LOG_SEV(lg(), ores::utility::log::error) << "Error loading history: " << error_msg.toStdString();

    emit errorOccurred(QString("Failed to load currency history: %1").arg(error_msg));
    MessageBoxHelper::critical(this, "History Load Error",
        QString("Failed to load currency history:\n%1").arg(error_msg));
}

void CurrencyHistoryDialog::onVersionSelected(int index) {
    if (index < 0 || index >= static_cast<int>(history_.versions.size())) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Version selected: " << index;

    displayChangesTab(index);
    displayFullDetailsTab(index);
}

void CurrencyHistoryDialog::displayChangesTab(int version_index) {
    ui_->changesTableWidget->setRowCount(0);

    if (version_index >= static_cast<int>(history_.versions.size())) {
        return;
    }

    const auto& current = history_.versions[version_index];

    // If this is the first (oldest) version, there's nothing to diff against
    // so leave the changes table empty
    if (version_index == static_cast<int>(history_.versions.size()) - 1) {
        return;
    }

    // Calculate diff with previous version
    const auto& previous = history_.versions[version_index + 1];
    auto diffs = calculateDiff(current, previous);

    ui_->changesTableWidget->setRowCount(diffs.size());

    for (int i = 0; i < diffs.size(); ++i) {
        const auto& [field, values] = diffs[i];
        const auto& [old_val, new_val] = values;

        auto* fieldItem = new QTableWidgetItem(field);
        auto* oldItem = new QTableWidgetItem(old_val);
        auto* newItem = new QTableWidgetItem(new_val);

        ui_->changesTableWidget->setItem(i, 0, fieldItem);
        ui_->changesTableWidget->setItem(i, 1, oldItem);
        ui_->changesTableWidget->setItem(i, 2, newItem);
    }
}

void CurrencyHistoryDialog::displayFullDetailsTab(int version_index) {
    if (version_index >= static_cast<int>(history_.versions.size())) {
        return;
    }

    const auto& version = history_.versions[version_index];
    const auto& data = version.data;

    ui_->isoCodeValue->setText(QString::fromStdString(data.iso_code));
    ui_->nameValue->setText(QString::fromStdString(data.name));
    ui_->numericCodeValue->setText(QString::fromStdString(data.numeric_code));
    ui_->symbolValue->setText(QString::fromStdString(data.symbol));
    ui_->fractionSymbolValue->setText(QString::fromStdString(data.fraction_symbol));
    ui_->fractionsPerUnitValue->setText(QString::number(data.fractions_per_unit));
    ui_->versionNumberValue->setText(QString::number(version.version_number));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->modifiedAtValue->setText(QString::fromStdString(version.modified_at));
}

QVector<QPair<QString, QPair<QString, QString>>>
CurrencyHistoryDialog::calculateDiff(
    const risk::domain::currency_version& current,
    const risk::domain::currency_version& previous) {

    QVector<QPair<QString, QPair<QString, QString>>> diffs;

    // Compare each field
    if (current.data.iso_code != previous.data.iso_code) {
        diffs.append({"ISO Code", {QString::fromStdString(previous.data.iso_code),
                                   QString::fromStdString(current.data.iso_code)}});
    }

    if (current.data.name != previous.data.name) {
        diffs.append({"Name", {QString::fromStdString(previous.data.name),
                               QString::fromStdString(current.data.name)}});
    }

    if (current.data.numeric_code != previous.data.numeric_code) {
        diffs.append({"Numeric Code", {QString::fromStdString(previous.data.numeric_code),
                                       QString::fromStdString(current.data.numeric_code)}});
    }

    if (current.data.symbol != previous.data.symbol) {
        diffs.append({"Symbol", {QString::fromStdString(previous.data.symbol),
                                 QString::fromStdString(current.data.symbol)}});
    }

    if (current.data.fraction_symbol != previous.data.fraction_symbol) {
        diffs.append({"Fraction Symbol", {QString::fromStdString(previous.data.fraction_symbol),
                                          QString::fromStdString(current.data.fraction_symbol)}});
    }

    if (current.data.fractions_per_unit != previous.data.fractions_per_unit) {
        diffs.append({"Fractions Per Unit", {QString::number(previous.data.fractions_per_unit),
                                             QString::number(current.data.fractions_per_unit)}});
    }

    if (current.data.rounding_type != previous.data.rounding_type) {
        diffs.append({"Rounding Type", {QString::fromStdString(previous.data.rounding_type),
                                        QString::fromStdString(current.data.rounding_type)}});
    }

    if (current.data.rounding_precision != previous.data.rounding_precision) {
        diffs.append({"Rounding Precision", {QString::number(previous.data.rounding_precision),
                                             QString::number(current.data.rounding_precision)}});
    }

    if (current.data.format != previous.data.format) {
        diffs.append({"Format", {QString::fromStdString(previous.data.format),
                                 QString::fromStdString(current.data.format)}});
    }

    if (current.data.currency_type != previous.data.currency_type) {
        diffs.append({"Currency Type", {QString::fromStdString(previous.data.currency_type),
                                        QString::fromStdString(current.data.currency_type)}});
    }

    return diffs;
}

QSize CurrencyHistoryDialog::sizeHint() const {
    if (!ui_->versionListWidget) {
        return QWidget::sizeHint();
    }

    // Calculate width based on version table columns plus changes/details tabs
    int versionTableWidth = ui_->versionListWidget->verticalHeader()->width();
    for (int i = 0; i < ui_->versionListWidget->horizontalHeader()->count(); ++i) {
        versionTableWidth += ui_->versionListWidget->columnWidth(i);
    }
    versionTableWidth += ui_->versionListWidget->verticalScrollBar()->sizeHint().width();
    versionTableWidth += ui_->versionListWidget->frameWidth() * 2;

    // Changes table width
    int changesTableWidth = 0;
    if (ui_->changesTableWidget) {
        changesTableWidth = ui_->changesTableWidget->verticalHeader()->width();
        for (int i = 0; i < ui_->changesTableWidget->horizontalHeader()->count(); ++i) {
            changesTableWidth += ui_->changesTableWidget->columnWidth(i);
        }
        changesTableWidth += ui_->changesTableWidget->verticalScrollBar()->sizeHint().width();
        changesTableWidth += ui_->changesTableWidget->frameWidth() * 2;
    }

    // Use the wider of the two tables, plus some padding
    int width = qMax(versionTableWidth, changesTableWidth) + 40;

    // Height: version table height + tab widget height
    int versionTableHeight = ui_->versionListWidget->horizontalHeader()->height();
    int rowHeight = ui_->versionListWidget->verticalHeader()->defaultSectionSize();
    versionTableHeight += rowHeight * qMin(10, ui_->versionListWidget->rowCount()); // Up to 10 rows
    versionTableHeight += ui_->versionListWidget->frameWidth() * 2;

    int tabHeight = 400; // Reasonable height for changes/details tabs
    int height = versionTableHeight + tabHeight + 40; // Extra padding

    return QSize(width, height);
}

}
