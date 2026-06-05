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
#include "ores.qt/CountryHistoryDialog.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/protocol.hpp"
#include <QLabel>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

namespace {

QString formatImageId(const std::optional<boost::uuids::uuid>& id) {
    if (!id.has_value())
        return QStringLiteral("(none)");
    return QString::fromStdString(boost::uuids::to_string(*id));
}

}

CountryHistoryDialog::CountryHistoryDialog(QString alpha2_code,
                                           ClientManager* clientManager,
                                           QWidget* parent)
    : HistoryDialogBase(parent)
    , ui_(new Ui::CountryHistoryDialog)
    , clientManager_(clientManager)
    , imageCache_(nullptr)
    , alpha2Code_(std::move(alpha2_code)) {

    BOOST_LOG_SEV(lg(), info) << "Creating country history widget for: "
                              << alpha2Code_.toStdString();

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    initializeHistoryUi({.versionList = ui_->versionListWidget,
                         .changesTable = ui_->changesTableWidget,
                         .titleLabel = ui_->titleLabel,
                         .closeButton = ui_->closeButton});
}

void CountryHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading country history for: "
                              << alpha2Code_.toStdString();

    refdata::messaging::get_country_history_request request;
    request.alpha2_code = alpha2Code_.toStdString();

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

int CountryHistoryDialog::historySize() const {
    return static_cast<int>(history_.size());
}

HistoryDialogBase::VersionRow CountryHistoryDialog::versionRow(int index) const {
    const auto& country = history_[index];
    return {.version = country.version,
            .cells = {relative_time_helper::format(country.recorded_at),
                      QString::fromStdString(country.modified_by),
                      QString::fromStdString(country.change_reason_code),
                      QString::fromStdString(country.change_commentary)}};
}

QString CountryHistoryDialog::historyTitle() const {
    const auto& latest = history_.front();
    return QString("Country History: %1 - %2")
        .arg(alpha2Code_)
        .arg(QString::fromStdString(latest.name));
}

HistoryDialogBase::DiffResult
CountryHistoryDialog::calculateDiffAt(int current_index,
                                      int previous_index) const {
    const auto& current = history_[current_index];
    const auto& previous = history_[previous_index];

    DiffResult diffs;
    checkString(diffs, "Alpha-2 Code", current.alpha2_code, previous.alpha2_code);
    checkString(diffs, "Alpha-3 Code", current.alpha3_code, previous.alpha3_code);
    checkString(diffs, "Numeric Code", current.numeric_code, previous.numeric_code);
    checkString(diffs, "Name", current.name, previous.name);
    checkString(diffs, "Official Name", current.official_name, previous.official_name);
    checkString(diffs, "Change Reason", current.change_reason_code,
                previous.change_reason_code);
    checkString(diffs, "Commentary", current.change_commentary,
                previous.change_commentary);

    if (current.image_id != previous.image_id) {
        diffs.append({"Flag",
                      {formatImageId(previous.image_id),
                       formatImageId(current.image_id)}});
    }

    return diffs;
}

QWidget* CountryHistoryDialog::changeCellWidget(const QString& field,
                                                const QString& value) {
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

void CountryHistoryDialog::displayFullDetails(int index) {
    const auto& country = history_[index];

    ui_->alpha2CodeValue->setText(QString::fromStdString(country.alpha2_code));
    ui_->alpha3CodeValue->setText(QString::fromStdString(country.alpha3_code));
    ui_->numericCodeValue->setText(QString::fromStdString(country.numeric_code));
    ui_->nameValue->setText(QString::fromStdString(country.name));
    ui_->officialNameValue->setText(QString::fromStdString(country.official_name));
    ui_->versionNumberValue->setText(QString::number(country.version));
    ui_->modifiedByValue->setText(QString::fromStdString(country.modified_by));
    ui_->recordedAtValue->setText(relative_time_helper::format(country.recorded_at));
}

void CountryHistoryDialog::openVersionAt(int index) {
    const auto& country = history_[index];
    BOOST_LOG_SEV(lg(), info) << "Opening country version " << country.version
                              << " in read-only mode";
    emit openVersionRequested(country, country.version);
}

void CountryHistoryDialog::revertToVersionAt(int index) {
    // The base has already confirmed with the user; revert TO the version
    // older than the selection, stamped with the latest version number.
    const auto& previous = history_[index + 1];

    BOOST_LOG_SEV(lg(), info) << "Requesting revert to version "
                              << previous.version;

    refdata::domain::country countryToRevert = previous;
    countryToRevert.version = history_.front().version;
    emit revertVersionRequested(countryToRevert);
}

void CountryHistoryDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
}

}
