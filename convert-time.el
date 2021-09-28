;;; convert-time.el --- convert time between zones     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/emacs-convert-time
;; Package-Requires: ((emacs "24.4"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package offer utility to convert times between zones as well as other utilities.

;; 

;;; Code:
(require 'org)
(defcustom convert-time--time-zones-alist nil
  "Assoc list of month names."
  :type 'alist
  :group 'convert-time)

(defcustom convert-time--times-list nil
  "Times to convert."
  :type 'list
  :group 'convert-time)

(defcustom convert-time--day-names-list '("SUN" "MON" "TUE" "WED" "THU" "FRI" "SAT")
  "Abbreviation for local time zone."
  :type 'list
  :group 'convert-time)

(defcustom convert-time--month-names-list '("JAN" "FEB" "MARCH" "APR" "MAY" "JUNE" "JULY" "AUG" "SEP" "OCT" "NOV" "DEC")
  "Abbreviation for local time zone."
  :type 'list
  :group 'convert-time)

(setq convert-time--time-zones-alist '(("EST" . "-0500")
				       ("CST" . "-0600")
				       ("CDT" . "-0500")
				       ("EET" . "+0200")
				       ("CET" . "+0100")
				       ("IST" . "+0530")
				       ("KOLKATA" . "+0530")
				       ("BENGALURU" . "+0530")
				       ("DELHI" . "+0530")
				       ("MUMBAI" . "+0530")
				       ("CHENNAI" . "+0530")
				       ("JST" . "+0900")
				       ("TOKYO" . "+0900")
				       ("BST" . "+0100")
				       ("CEST" . "+0200")))

(setq convert-time--times-list '("Right Now"
				 "9:00 AM"
				 "9:30 AM"
				 "10:00 AM"
				 "10:30 AM"
				 "11:00 AM"
				 "11:30 AM"
				 "12:00 PM"
				 "1:00 PM"
				 "1:30 PM"
				 "2:00 PM"
				 "2:30 PM"
				 "3:00 PM"
				 "3:30 PM"
				 "4:00 PM"
				 "4:30 PM"
				 "5:00 PM"
				 "5:30 PM"
				 "6:00 PM"
				 "6:30 PM"
				 "7:00 PM"
				 "7:30 PM"
				 "8:00 PM"
				 "8:30 PM"
				 "9:00 PM"
				 "9:30 PM"
				 "10:00 PM"
				 "10:30 PM"
				 "11:00 PM"
				 "11:30 PM"))

(defun convert-time--get-day-name (day month year)
  "Get the day name from DAY MONTH and YEAR."
  (nth (nth 6 (decode-time (encode-time (list 0 0 0 day month year nil nil nil)))) convert-time--day-names-list))

(defun convert-time--get-day-month-year-number-from-date (date separator day-position month-position year-position)
  "Get day, month and year numbers from a DATE with SEPARATOR.  DAY-POSITION, MONTH-POSITION and YEAR-POSITION helps to recognize the format of date."
  (let* ((splitted-strings (split-string date separator))
	 (day (string-to-number (nth day-position splitted-strings)))
	 (month (string-to-number (nth month-position splitted-strings)))
	 (year (string-to-number (nth year-position splitted-strings))))
    (list day month year)))

(defun convert-time--get-day-name-from-date (date separator day-position month-position year-position)
  "Get day name from a DATE with SEPARATOR.  DAY-POSITION, MONTH-POSITION and YEAR-POSITION helps to recognize the format of date."
  (let* ((splitted-strings (split-string date separator))
	 (day (string-to-number (nth day-position splitted-strings)))
	 (month (string-to-number (nth month-position splitted-strings)))
	 (year (string-to-number (nth year-position splitted-strings))))
    (convert-time--get-day-name day month year)))

(defun convert-time--get-month-name-from-date (date separator month-position)
  "Get month name from a DATE with SEPARATOR.  MONTH-POSITION is the position of month number in the date."
  (let* ((splitted-strings (split-string date separator))
	 (month (string-to-number (nth month-position splitted-strings))))
    (nth (1- month) convert-time--month-names-list)))

(defun convert-time--get-year-from-date (date separator year-position)
  "Get year from a DATE with SEPARATOR.  YEAR-POSITION is the position of month number in the date."
  (let* ((splitted-strings (split-string date separator)))
    (string-to-number (nth year-position splitted-strings))))

(defun convert-time--get-day-from-date (date separator day-position)
  "Get day number from a DATE with SEPARATOR.  DAY-POSITION is the position of month number in the date."
  (let* ((splitted-strings (split-string date separator)))
    (string-to-number (nth day-position splitted-strings))))

(defun convert-time--get-next-or-previous-date (date separator day-position month-position year-position delta)
  "Get the next or previous date with DELTA time difference for a DATE with SEPARATOR.  DAY-POSITION, MONTH-POSITION and YEAR-POSITION helps to recognize the format of date."
  (let* ((day-month-year (convert-time--get-day-month-year-number-from-date date separator day-position month-position year-position))
	 (day (nth 0 day-month-year))
	 (month (nth 1 day-month-year))
	 (year (nth 2 day-month-year))
	 (org-date-string (org-read-date nil nil delta nil (encode-time (list 0 0 0 day month year nil nil nil))))
	 (org-date-strings-list (split-string org-date-string "-"))
	 (org-year (nth 0 org-date-strings-list))
	 (org-month (nth 1 org-date-strings-list))
	 (org-day (nth 2 org-date-strings-list))
	 (next-date-string-list '(0 0 0)))
    (setf (nth day-position next-date-string-list) org-day)
    (setf (nth month-position next-date-string-list) org-month)
    (setf (nth year-position next-date-string-list) org-year)
    (string-join next-date-string-list (if (string-equal separator "\\.") "." separator))))

(defun convert-time--get-converted-time (time-to-convert from-zone-u to-zone-u)
  "Get converted time for TIME-TO-CONVERT from FROM-ZONE-U to another TO-ZONE-U."
  (let* ((days convert-time--day-names-list)
	 (time (parse-time-string (if (string-equal time-to-convert "Right Now")
				      (current-time-string)
				    time-to-convert)))
	 (from-sec (nth 0 time))
	 (from-min (nth 1 time))
	 (from-hour (nth 2 time))
	 (from-day (nth 6 time)))
    (when (string-match-p (regexp-quote "PM") (upcase time-to-convert))
      (setq  from-hour (+ 12 from-hour)))
    (let* ((time-shift (- (nth 5 (parse-time-string (cdr (assoc to-zone-u convert-time--time-zones-alist))))
			  (nth 5 (parse-time-string (cdr (assoc from-zone-u convert-time--time-zones-alist))))))
	   (hour-shift (/ time-shift 100))
	   (min-shift (% time-shift 100))
	   (to-min (+ from-min min-shift))
	   (to-hour (+ from-hour hour-shift))
	   (to-day-name "")
	   A-or-P)
      (cond ((< to-min 0) (progn
			    (setq to-min (+ to-min 60))
			    (setq to-hour (1- to-hour))))
	     ((= to-min 60) (progn
			     (setq to-min 0)
			     (setq to-hour (1+ to-hour))))
	     ((> to-min 60) (progn
			     (setq to-min (- to-min 60))
			     (setq to-hour (1+ to-hour)))))
      (cond ((>= to-hour 24) (progn
			       (setq to-hour (- to-hour 24))
			       (if (not (equal from-day nil))
				   (setq to-day-name (nth (1+ from-day) days))
				 (setq to-day-name "+1d"))))
	    ((< to-hour 0) (progn
			     (setq to-hour (+ 24 to-hour))
			     (if (not (equal from-day nil))
				 (setq to-day-name (nth (1- from-day) days))
			       (setq to-day-name "-1d"))))
	    ((and (> to-hour 0) (< to-hour 24)) (if (not (equal from-day nil))
						    (setq to-day-name (nth from-day days))
						  (setq to-day-name "+0d"))))
      (cond ((and (= to-hour 0) (= to-min 0)) (setq A-or-P "Midnight"))
	    ((< to-hour 12) (setq A-or-P "AM"))
	    ((and (= to-hour 12) (= to-min 0)) (setq A-or-P "Noon"))
	    ((or (> to-hour 12) (and (= to-hour 12) (> to-min 0))) (progn
			      (setq to-hour (- to-hour 12))
			      (setq A-or-P "PM"))))
      (list to-min to-hour A-or-P to-day-name))))

(defun convert-time--convert-time (time-to-convert from-zone-u to-zone-u)
  "Convert TIME-TO-CONVERT from FROM-ZONE-U to another TO-ZONE-U."
  (interactive
   (let* ((time-to-convert (completing-read "Enter time to convert: " convert-time--times-list))
	  (from-zone-u (if (string-equal time-to-convert "Right Now")
			   (nth 1 (current-time-zone))
			 (upcase (completing-read "Enter from zone: " convert-time--time-zones-alist))))
	  (to-zone-u (upcase (completing-read (format "Convert %s from %s to: " time-to-convert from-zone-u) convert-time--time-zones-alist))))
     (list time-to-convert from-zone-u to-zone-u)))
  (let* ((converted-list (convert-time--get-converted-time time-to-convert from-zone-u to-zone-u))
	 (to-min (nth 0 converted-list))
	 (to-hour (nth 1 converted-list))
	 (A-or-P (nth 2 converted-list))
	 (to-day-name (nth 3 converted-list)))
    (message (format "%s %s = %.2d:%.2d %s %s %s" (if (string-equal time-to-convert "Right Now") (current-time-string) (upcase time-to-convert)) from-zone-u to-hour to-min (upcase A-or-P) (upcase to-day-name) to-zone-u))))

(provide 'convert-time)
;;; convert-time.el ends here
