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
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.dq.api/domain/change_reason.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/FlagSelectorDialog.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include <QHBoxLayout>
#include <QIcon>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QSize>
#include <QTabWidget>
#include <QWidget>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace {
constexpr auto kFlagStyleDefault =
    "QPushButton { border: none; background: transparent; padding: 0px; } "
    "QPushButton:hover { background: rgba(255, 255, 255, 15); }";
constexpr auto kFlagStyleChanged =
    "QPushButton { border: 2px solid orange; background: transparent; "
    "padding: 0px; border-radius: 4px; } "
    "QPushButton:hover { background: rgba(255, 255, 255, 15); }";
}

namespace ores::qt {

// MOC requires a non-inline destructor for proper vtable generation
DetailDialogBase::~DetailDialogBase() {}

void DetailDialogBase::setProvenanceEnabled(bool enabled) {
    auto* tw = tabWidget();
    auto* pt = provenanceTab();
    if (!tw || !pt)
        return;

    for (int i = 0; i < tw->count(); ++i) {
        if (tw->widget(i) == pt) {
            tw->setTabEnabled(i, enabled);
            return;
        }
    }
}

void DetailDialogBase::populateProvenance(int version,
                                          const std::string& modified_by,
                                          const std::string& performed_by,
                                          std::chrono::system_clock::time_point recorded_at,
                                          const std::string& change_reason_code,
                                          const std::string& change_commentary) {
    auto* pw = provenanceWidget();
    if (!pw)
        return;
    pw->populate(
        version, modified_by, performed_by, recorded_at, change_reason_code, change_commentary);
}

void DetailDialogBase::clearProvenance() {
    auto* pw = provenanceWidget();
    if (!pw)
        return;
    pw->clear();
}

std::optional<DetailDialogBase::change_reason_selection> DetailDialogBase::promptChangeReason(
    ChangeReasonDialog::OperationType opType, bool isDirty, std::string_view category) {
    if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
        emit errorMessage(tr("Change reasons not loaded. Please try again."));
        return std::nullopt;
    }

    const auto cat = std::string{category};
    std::vector<dq::domain::change_reason> reasons;
    switch (opType) {
        case ChangeReasonDialog::OperationType::Create:
            reasons = changeReasonCache_->getReasonsForNew(cat);
            break;
        case ChangeReasonDialog::OperationType::Amend:
            reasons = changeReasonCache_->getReasonsForAmend(cat);
            break;
        case ChangeReasonDialog::OperationType::Delete:
            reasons = changeReasonCache_->getReasonsForDelete(cat);
            break;
    }

    if (reasons.empty()) {
        emit errorMessage(tr("No change reasons available. Please contact administrator."));
        return std::nullopt;
    }

    ChangeReasonDialog dlg(reasons, opType, isDirty, this);
    if (dlg.exec() != QDialog::Accepted)
        return std::nullopt;

    return change_reason_selection{dlg.selectedReasonCode(), dlg.commentary()};
}

void DetailDialogBase::onCloseClicked() {
    if (hasUnsavedChanges()) {
        auto reply = MessageBoxHelper::question(this,
                                                tr("Unsaved Changes"),
                                                tr("You have unsaved changes. Close anyway?"),
                                                QMessageBox::Yes | QMessageBox::No);
        if (reply != QMessageBox::Yes)
            return;
    }
    closeConfirmed_ = true;
    requestClose();
}

void DetailDialogBase::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    if (!imageCache_)
        return;
    connect(imageCache_, &ImageCache::imagesLoaded, this, &DetailDialogBase::updateFlagDisplay);
    connect(imageCache_, &ImageCache::allLoaded, this, &DetailDialogBase::updateFlagDisplay);
}

void DetailDialogBase::initFlagButton(QLayout* container) {
    if (!container)
        return;

    flagButton_ = new QPushButton(this);
    flagButton_->setFixedSize(52, 52);
    flagButton_->setIconSize(QSize(48, 48));
    flagButton_->setFlat(true);
    flagButton_->setStyleSheet(kFlagStyleDefault);
    flagButton_->setCursor(Qt::PointingHandCursor);
    flagButton_->setToolTip(tr("Click to select flag"));
    connect(flagButton_, &QPushButton::clicked, this, &DetailDialogBase::onSelectFlagClicked);

    auto* wrapper = new QWidget(this);
    auto* layout = new QHBoxLayout(wrapper);
    layout->setContentsMargins(0, 4, 0, 4);
    layout->addStretch();
    layout->addWidget(flagButton_);
    layout->addStretch();
    container->addWidget(wrapper);

    initKeyFlagField();
    updateFlagDisplay();
}

void DetailDialogBase::initKeyFlagField() {
    // Inline flag inside the key field tracks the typed value live. Safe to
    // call standalone (no flag button): an entity with only a derived,
    // read-only key-field icon and no uploadable image_id of its own (e.g.
    // currency_pair_convention) calls just this; initFlagButton() (above)
    // calls it too for entities with both.
    if (auto* edit = keyFlagField()) {
        connect(edit, &QLineEdit::textChanged, this, &DetailDialogBase::updateFlagDisplay);
    }
    updateFlagDisplay();
}

void DetailDialogBase::onSelectFlagClicked() {
    if (!imageCache_) {
        emit errorMessage(tr("Image cache not available"));
        return;
    }

    QString current = pendingImageId_;
    if (current.isEmpty()) {
        if (const auto id = entityImageId())
            current = QString::fromStdString(boost::uuids::to_string(*id));
    }

    FlagSelectorDialog dialog(imageCache_, current, this);
    if (dialog.exec() != QDialog::Accepted)
        return;

    pendingImageId_ = dialog.selectedImageId();
    flagChanged_ = true;
    updateFlagDisplay();
    emit flagEdited();
    emit statusMessage(tr("Flag changed. Click Save to apply."));
}

void DetailDialogBase::updateFlagDisplay() {
    if (flagButton_) {
        flagButton_->setStyleSheet(flagChanged_ ? kFlagStyleChanged : kFlagStyleDefault);
        flagButton_->setToolTip(flagChanged_ ? tr("Flag changed (unsaved)") :
                                               tr("Click to select flag"));
        if (imageCache_) {
            QString idStr;
            if (flagChanged_)
                idStr = pendingImageId_;
            else if (const auto id = entityImageId())
                idStr = QString::fromStdString(boost::uuids::to_string(*id));

            QIcon buttonIcon;
            if (!idStr.isEmpty())
                buttonIcon = imageCache_->getIcon(idStr.toStdString());
            if (buttonIcon.isNull())
                buttonIcon = imageCache_->getNoFlagIcon();
            if (!buttonIcon.isNull())
                flagButton_->setIcon(buttonIcon);
        }
    }

    // Inline flag inside the key line-edit (entity-specific accessor) —
    // independent of flagButton_: a derived, read-only icon needs no
    // uploadable image_id of its own.
    if (!imageCache_)
        return;
    if (auto* edit = keyFlagField()) {
        const auto key = edit->text().trimmed().toStdString();
        set_line_edit_flag_icon(edit, keyFlagIcon(key), keyFlagAction_);
    }
}

std::optional<boost::uuids::uuid> DetailDialogBase::selectedImageId() const {
    if (pendingImageId_.isEmpty())
        return std::nullopt;
    try {
        return boost::uuids::string_generator{}(pendingImageId_.toStdString());
    } catch (...) { // malformed id from the selector must not crash save
        return std::nullopt;
    }
}

}
